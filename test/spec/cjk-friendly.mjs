// Render the test cases of markdown-cjk-friendly with its remark plugins.
//
// Usage: npm install && node cjk-friendly.mjs TESTCASES-DIR > cjk-friendly-e9762c3.json
//
// TESTCASES-DIR is testcases of https://github.com/tats-u/markdown-cjk-friendly
// at e9762c379b36ce5ee0e9b185fae9a908244f598b, whose plugins package.json pins.
// Each paragraph of a test case file becomes an example.

import { readFileSync } from "node:fs";
import { join } from "node:path";
import rehypeStringify from "rehype-stringify";
import remarkCjkFriendly from "remark-cjk-friendly";
import remarkCjkFriendlyGfmStrikethrough from "remark-cjk-friendly-gfm-strikethrough";
import remarkGfm from "remark-gfm";
import remarkParse from "remark-parse";
import remarkRehype from "remark-rehype";
import { unified } from "unified";

const FILES = ["strong.md", "korean.md", "pseudo-emoji.md", "non-bmp.md",
               "underscore-cjk-punct.md", "gfm-strikethrough.md", "gfm-non-cjk.md"];

const processor = unified()
  .use(remarkParse)
  .use(remarkGfm)
  .use(remarkCjkFriendly)
  .use(remarkCjkFriendlyGfmStrikethrough)
  .use(remarkRehype)
  .use(rehypeStringify);

const examples = FILES.flatMap((file) =>
  readFileSync(join(process.argv[2], file), "utf-8")
    .split(/\n\n+/)
    .filter((paragraph) => paragraph.trim())
    .map((paragraph) => {
      const markdown = `${paragraph}\n`;
      return { file, markdown, html: `${processor.processSync(markdown)}\n` };
    }));

console.log(JSON.stringify(examples, null, 1));
