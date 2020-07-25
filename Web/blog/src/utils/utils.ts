/** Only supports a tiny subset of minification.
 * Meant for when you've written JS in a multiline string literal.
 */
export function minifyJs(js: string): string {
  return js
    .trim()
    .replace(/(?<=[;,}])\s+/gm, "")
    .replace(/;$/, "")
}

// Shitty hack to ensure exhaustivity
export function exhaustiveCheck(arg: never): void {}
