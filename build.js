const esbuild = require("esbuild");
const PureScriptPlugin = require("esbuild-plugin-purescript");
const path = require("path");
const fs = require("fs");

esbuild
  .build({
    entryPoints: ["src/index.js"],
    bundle: true,
    minify: true,
    treeShaking: true,
    outdir: "dist",
    plugins: [
      PureScriptPlugin({
        output: path.resolve(
          __dirname,
          fs.existsSync("dce-output") ? "dce-output" : "output"
        ),
      }),
    ],
  })
  .catch((_e) => process.exit(1));
