// swift-tools-version: 6.0
import PackageDescription

let package = Package(
  name: "Vox",
  platforms: [.macOS(.v12)],
  products: [
    .library(
      name: "Vox",
      type: .dynamic,
      targets: ["Vox"])
  ],
  dependencies: [
    .package(
      url: "https://github.com/SavchenkoValeriy/emacs-swift-module.git",
      revision: "c776706c9338b8ba72a76a2128fd89bd4ac4269e")
  ],
  targets: [
    .target(
      name: "Vox",
      dependencies: [
        .product(name: "EmacsSwiftModule", package: "emacs-swift-module")
      ],
      linkerSettings: [
        .linkedFramework("AVFoundation"),
        .linkedFramework("CoreAudio"),
      ],
      plugins: [
        .plugin(name: "ModuleFactoryPlugin", package: "emacs-swift-module")
      ]
    )
  ]
)
