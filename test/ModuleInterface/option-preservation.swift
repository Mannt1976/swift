// REQUIRES: OS=macosx
// RUN: %target-swift-frontend -enable-resilience -target arm64-apple-ios8.0 -emit-interface-path %t.swiftinterface -module-name t -emit-module -o /dev/null %s
// RUN: %FileCheck %s < %t.swiftinterface -check-prefix=CHECK-SWIFTINTERFACE
// RUN: %target-swift-frontend -emit-module -o %t.swiftmodule -module-name t %t.swiftinterface
// RUN: llvm-bcanalyzer -dump %t.swiftmodule | %FileCheck %s -check-prefix=CHECK-SWIFTMODULE
//
// CHECK-SWIFTINTERFACE: {{swift-module-flags:.* -enable-resilience -target arm64-apple-ios8.0}}
// CHECK-SWIFTMODULE: {{TARGET abbrevid.* data = 'arm64-apple-ios8.0'}}
// CHECK-SWIFTMODULE: {{RESILIENCE_STRATEGY abbrevid.* op0=1}}

public func foo() { }
