const esbuild = require("esbuild");
const PureScriptPlugin = require("esbuild-plugin-purescript");
const path = require("path");

esbuild
  .build({
    entryPoints: ["src/index.js"],
    bundle: true,
    minify: true,
    outdir: "dist",
    plugins: [
      PureScriptPlugin(),
    ],
  })
  .catch((_e) => process.exit(1));
