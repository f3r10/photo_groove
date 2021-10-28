import * as elmTailwindModules from "elm-tailwind-modules"
import tailwindConfig from "./tailwind.config.js"
import autoprefixer from "autoprefixer"
import postcssImport from "postcss-import"
import * as postcss from "postcss"
import tailwindcss from "tailwindcss"
import { promises as fs } from "fs"

const etmConfig = {
    directory: "gen/",
    moduleName: "Tailwind",
    generateDocumentation: true,
    // custom stuff
    inputCssFile: "./global.css",
    outputCssFile: "public/application.css",
}

const addLogPrefix = line => `[elm-tailwind-modules] ${line}`
const logFunction = message => console.log(message.split("\n").map(addLogPrefix).join("\n"))

const elmTailwindModulesPlugin = elmTailwindModules.asPostcssPlugin({
    moduleName: etmConfig.moduleName,
    tailwindConfig: tailwindConfig,
    generateDocumentation: etmConfig.generateDocumentation,
    logFunction,
    modulesGeneratedHook: async generated => elmTailwindModules.writeGeneratedFiles({
        directory: etmConfig.directory,
        moduleName: etmConfig.moduleName,
        logFunction,
        generated
    })
});

async function genCss() {
    const inputCss = await fs.readFile(etmConfig.inputCssFile, { encoding: "utf8" })

    const result = await postcss.default([
        postcssImport,
        tailwindcss(tailwindConfig),
        autoprefixer,
        elmTailwindModulesPlugin
    ]).process(inputCss, {
        from: etmConfig.inputCssFile,
        to: etmConfig.outputCssFile,
    })

    logFunction(`Saving remaining global css to ${etmConfig.outputCssFile}`)
    await fs.writeFile(etmConfig.outputCssFile, result.content)
}

genCss()
    .catch(console.error)
    .then(() => console.log("finish"))
