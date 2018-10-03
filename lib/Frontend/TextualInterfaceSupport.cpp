//===--- TextualInterfaceSupport.cpp - swiftinterface files ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/TextualInterfaceSupport.h"
#include "swift/FrontendTool/FrontendTool.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Module.h"
#include "clang/Basic/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/CrashRecoveryContext.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/StringSaver.h"

using namespace swift;

#define SWIFT_TOOLS_VERSION_KEY "swift-tools-version"
#define SWIFT_MODULE_FLAGS_KEY "swift-module-flags"

static bool
extractSwiftInterfaceVersionAndArgs(DiagnosticEngine &Diags,
                                    clang::vfs::FileSystem &FS,
                                    StringRef SwiftInterfacePathIn,
                                    swift::version::Version &Vers,
                                    llvm::StringSaver &SubArgSaver,
                                    SmallVectorImpl<const char *> &SubArgs) {
  auto FileOrError = swift::vfs::getFileOrSTDIN(FS, SwiftInterfacePathIn);
  if (!FileOrError) {
    Diags.diagnose(SourceLoc(), diag::error_open_input_file,
                   SwiftInterfacePathIn, FileOrError.getError().message());
    return true;
  }
  auto SB = FileOrError.get()->getBuffer();
  auto VersRe = getSwiftInterfaceToolsVersionRegex();
  auto FlagRe = getSwiftInterfaceModuleFlagsRegex();
  SmallVector<StringRef, 1> VersMatches, FlagMatches;
  if (!VersRe.match(SB, &VersMatches)) {
    Diags.diagnose(SourceLoc(),
                   diag::error_extracting_version_from_textual_interface);
    return true;
  }
  if (!FlagRe.match(SB, &FlagMatches)) {
    Diags.diagnose(SourceLoc(),
                   diag::error_extracting_flags_from_textual_interface);
    return true;
  }
  assert(VersMatches.size() == 2);
  assert(FlagMatches.size() == 2);
  Vers = swift::version::Version(VersMatches[1], SourceLoc(), &Diags);
  llvm::cl::TokenizeGNUCommandLine(FlagMatches[1], SubArgSaver, SubArgs);
  return false;
}

/// Convert a .swiftinterface file to a .swiftmodule file
std::error_code TextualInterfaceModuleLoader::openModuleFiles(
    StringRef DirName, StringRef ModuleFilename, StringRef ModuleDocFilename,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    llvm::SmallVectorImpl<char> &Scratch) {

  std::error_code ErrorCode;
  auto &FS = *Ctx.SourceMgr.getFileSystem();
  auto &Diags = Ctx.Diags;
  auto &SearchPathOpts = Ctx.SearchPathOpts;

  llvm::SmallString<128> InPath, OutPath;
  InPath = DirName;
  OutPath = CacheDir;
  llvm::sys::path::append(InPath, ModuleFilename);
  // FIXME: this is not a good enough name.
  llvm::sys::path::append(OutPath, ModuleFilename);
  llvm::sys::path::replace_extension(InPath, file_types::getExtension(
                                      file_types::TY_SwiftModuleInterfaceFile));

  bool success = llvm::CrashRecoveryContext().RunSafelyOnThread([&] {
    llvm::BumpPtrAllocator SubArgsAlloc;
    llvm::StringSaver SubArgSaver(SubArgsAlloc);
    SmallVector<const char *, 16> SubArgs;
    swift::version::Version Vers;

    // Set up a sub-invocation to consume the .swiftinterface and emit the
    // .swiftmodule
    CompilerInvocation SubInvocation;
    SubInvocation.setImportSearchPaths(SearchPathOpts.ImportSearchPaths);
    SubInvocation.setFrameworkSearchPaths(SearchPathOpts.FrameworkSearchPaths);
    SubInvocation.setSDKPath(SearchPathOpts.SDKPath);
    SubInvocation.setInputKind(InputFileKind::SwiftModuleInterface);
    auto &FEOpts = SubInvocation.getFrontendOptions();
    FEOpts.RequestedAction = FrontendOptions::ActionType::EmitModuleOnly;
    FEOpts.InputsAndOutputs.addPrimaryInputFile(InPath);
    FEOpts.InputsAndOutputs.setMainAndSupplementaryOutputs({OutPath.str()}, {});

    // Add in any arguments that were saved-in and thus specified _by_ the
    // .swiftinterface
    if (extractSwiftInterfaceVersionAndArgs(Diags, FS, InPath, Vers,
                                            SubArgSaver, SubArgs)) {
      ErrorCode = std::make_error_code(std::errc::invalid_argument);
      return;
    }

    if (SubInvocation.parseArgs(SubArgs, Diags)) {
      ErrorCode = std::make_error_code(std::errc::invalid_argument);
      return;
    }

    // Build the .swiftmodule
    CompilerInstance SubInstance;
    int ReturnValue = 0;
    if (swift::performCompile(SubInstance, SubInvocation, SubArgs,
                              ReturnValue) ||
        ReturnValue != 0)
      ErrorCode = std::make_error_code(std::errc::invalid_argument);
  });
  if (!success)
    ErrorCode = std::make_error_code(std::errc::invalid_argument);

  // Finish off by delegating back up to the SerializedModuleLoaderBase
  // routine that can load the recently-manufactured serialized module.
  if (!ErrorCode) {
    ErrorCode = SerializedModuleLoaderBase::openModuleFiles(
        DirName, ModuleFilename, ModuleDocFilename, ModuleBuffer,
        ModuleDocBuffer, Scratch);
  }
  return ErrorCode;
}

/// Diagnose any scoped imports in \p imports, i.e. those with a non-empty
/// access path. These are not yet supported by textual interfaces, since the
/// information about the declaration kind is not preserved through the binary
/// serialization that happens as an intermediate step in non-whole-module
/// builds.
///
/// These come from declarations like `import class FooKit.MainFooController`.
static void diagnoseScopedImports(DiagnosticEngine &diags,
                                  ArrayRef<ModuleDecl::ImportedModule> imports){
  for (const ModuleDecl::ImportedModule &importPair : imports) {
    if (importPair.first.empty())
      continue;
    diags.diagnose(importPair.first.front().second,
                   diag::textual_interface_scoped_import_unsupported);
  }
}

/// Prints to \p out a comment containing a tool-versions identifier as well
/// as any relevant command-line flags in \p Opts used to construct \p M.
static void printToolVersionAndFlagsComment(raw_ostream &out,
                                            TextualInterfaceOptions const &Opts,
                                            ModuleDecl *M) {
  auto &Ctx = M->getASTContext();
  out << "// " SWIFT_TOOLS_VERSION_KEY ": "
      << Ctx.LangOpts.EffectiveLanguageVersion << "\n";
  out << "// " SWIFT_MODULE_FLAGS_KEY ": "
      << Opts.TextualInterfaceFlags << "\n";
}

llvm::Regex swift::getSwiftInterfaceToolsVersionRegex() {
  return llvm::Regex("^// " SWIFT_TOOLS_VERSION_KEY ": ([0-9\\.]+)$",
                     llvm::Regex::Newline);
}

llvm::Regex swift::getSwiftInterfaceModuleFlagsRegex() {
  return llvm::Regex("^// " SWIFT_MODULE_FLAGS_KEY ": (.*)$",
                     llvm::Regex::Newline);
}

/// Prints the imported modules in \p M to \p out in the form of \c import
/// source declarations.
static void printImports(raw_ostream &out, ModuleDecl *M) {
  // FIXME: This is very similar to what's in Serializer::writeInputBlock, but
  // it's not obvious what higher-level optimization would be factored out here.
  SmallVector<ModuleDecl::ImportedModule, 8> allImports;
  M->getImportedModules(allImports, ModuleDecl::ImportFilter::All);
  ModuleDecl::removeDuplicateImports(allImports);
  diagnoseScopedImports(M->getASTContext().Diags, allImports);

  // Collect the public imports as a subset so that we can mark them with
  // '@_exported'.
  SmallVector<ModuleDecl::ImportedModule, 8> publicImports;
  M->getImportedModules(publicImports, ModuleDecl::ImportFilter::Public);
  llvm::SmallSet<ModuleDecl::ImportedModule, 8,
                 ModuleDecl::OrderImportedModules> publicImportSet;
  publicImportSet.insert(publicImports.begin(), publicImports.end());

  for (auto import : allImports) {
    if (import.second->isStdlibModule() ||
        import.second->isOnoneSupportModule() ||
        import.second->isBuiltinModule()) {
      continue;
    }

    if (publicImportSet.count(import))
      out << "@_exported ";
    out << "import ";
    import.second->getReverseFullModuleName().printForward(out);

    // Write the access path we should be honoring but aren't.
    // (See diagnoseScopedImports above.)
    if (!import.first.empty()) {
      out << "/*";
      for (const auto &accessPathElem : import.first)
        out << "." << accessPathElem.first;
      out << "*/";
    }

    out << "\n";
  }
}

bool swift::emitModuleInterface(raw_ostream &out,
                                TextualInterfaceOptions const &Opts,
                                ModuleDecl *M) {
  assert(M);

  printToolVersionAndFlagsComment(out, Opts, M);
  printImports(out, M);

  const PrintOptions printOptions = PrintOptions::printTextualInterfaceFile();
  SmallVector<Decl *, 16> topLevelDecls;
  M->getTopLevelDecls(topLevelDecls);
  for (const Decl *D : topLevelDecls) {
    if (!D->shouldPrintInContext(printOptions))
      continue;
    D->print(out, printOptions);
    out << "\n";
  }
  return false;
}
