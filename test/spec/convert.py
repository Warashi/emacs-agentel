"""Turn the examples of the CommonMark and GFM specs, and the test cases of
markdown-cjk-friendly, into Lisp test cases.

Each example becomes a plist with the Markdown and the tree a parser is
expected to give for it, in the form

    (TYPE ATTRIBUTES . CHILDREN)

where ATTRIBUTES is a plist and text is a string.  The tree is read
from the HTML the spec expects.

Usage: python3 convert.py COMMONMARK-JSON GFM-SPEC-TXT CJK-FRIENDLY-JSON > expected.eld
"""

import json
import re
import sys
from html.parser import HTMLParser

# Examples whose expected HTML passes raw HTML through: a parser that does
# not take HTML as such gives another tree, and the tree of the HTML cannot
# tell what came from Markdown.
RAW_HTML_SECTIONS = {"HTML blocks", "Raw HTML"}
RAW_HTML_EXAMPLES = {21, 31, 201, 308, 309, 344, 475, 476, 477,
                     491, 494, 524, 536, 642, 643}
# CommonMark examples that GFM autolinks turn into links.
AUTOLINKED_EXAMPLES = {602, 606, 608, 611, 612}
# GFM extensions whose examples are kept.
GFM_EXTENSIONS = {"table", "strikethrough", "autolink", "disabled"}

BLOCKS = {"p", "h1", "h2", "h3", "h4", "h5", "h6", "hr", "blockquote",
          "ul", "ol", "li", "pre", "table", "thead", "tbody", "tr"}
VOID = {"hr", "br", "img", "input"}
ATTRIBUTES = {
    "a": {"href", "title"}, "img": {"src", "alt", "title"},
    "ol": {"start"}, "code": {"class"}, "th": {"align"}, "td": {"align"},
    "input": {"type", "checked", "disabled"},
}
KNOWN = BLOCKS | VOID | {"em", "strong", "del", "code", "a", "th", "td"}


class ConversionError(Exception):
    pass


class DomBuilder(HTMLParser):
    """Build a tree of (TAG, ATTRS, CHILDREN) from HTML, refusing the unknown."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.root = ("root", {}, [])
        self.stack = [self.root]

    def handle_starttag(self, tag, attrs):
        if tag not in KNOWN:
            raise ConversionError(f"unknown tag <{tag}>")
        attrs = dict(attrs)
        unknown = set(attrs) - ATTRIBUTES.get(tag, set())
        if unknown:
            raise ConversionError(f"unknown attributes {unknown} of <{tag}>")
        node = (tag, attrs, [])
        self.stack[-1][2].append(node)
        if tag not in VOID:
            self.stack.append(node)

    def handle_startendtag(self, tag, attrs):
        self.handle_starttag(tag, attrs)
        if tag not in VOID:
            self.stack.pop()

    def handle_endtag(self, tag):
        if tag in VOID:
            return
        if self.stack[-1][0] != tag:
            raise ConversionError(f"</{tag}> closes <{self.stack[-1][0]}>")
        self.stack.pop()

    def handle_data(self, data):
        children = self.stack[-1][2]
        if children and isinstance(children[-1], str):
            children[-1] += data
        else:
            children.append(data)

    def tree(self, html):
        self.feed(html)
        self.close()
        if len(self.stack) != 1:
            raise ConversionError(f"<{self.stack[-1][0]}> is not closed")
        return self.root


def is_block(node):
    return not isinstance(node, str) and node[0] in BLOCKS


def blocks(children):
    """Return the blocks of CHILDREN, dropping the newlines between them."""
    result = []
    for child in children:
        if isinstance(child, str):
            if child.strip("\n"):
                raise ConversionError(f"text {child!r} among blocks")
        else:
            result.append(block(child))
    return result


def inlines(children):
    """Return the inlines of CHILDREN, merging text and dropping the
    newline a hard break is followed by."""
    result = []
    after_break = False
    for child in children:
        if isinstance(child, str):
            if after_break and child.startswith("\n"):
                child = child[1:]
            if not child:
                continue
            if result and isinstance(result[-1], str):
                result[-1] += child
            else:
                result.append(child)
            after_break = False
        else:
            node = inline(child)
            result.append(node)
            after_break = node[0] == "hard-break"
    return result


def text_of(children):
    """Return the text of CHILDREN with their markup left out."""
    return "".join(c if isinstance(c, str) else text_of(c[2]) for c in children)


def item(node, tight):
    """Return the list item NODE, with its inlines in a paragraph if TIGHT."""
    _, _, children = node
    attrs = {}
    if children and not isinstance(children[0], str) and children[0][0] == "input":
        attrs["checked"] = "checked" in children[0][1]
        children = children[1:]
        if children and isinstance(children[0], str):
            children = [children[0].removeprefix(" ")] + children[1:]
    result = []
    run = []

    def flush():
        if run:
            content = inlines(run)
            if content and isinstance(content[0], str):
                content[0] = content[0].lstrip("\n")
            if content and isinstance(content[-1], str):
                content[-1] = content[-1].rstrip("\n")
            content = [c for c in content if c != ""]
            if content:
                result.append(("paragraph", {}, content))
            run.clear()

    for child in children:
        if is_block(child):
            flush()
            result.append(block(child))
        elif isinstance(child, str) and not tight:
            if child.strip("\n"):
                raise ConversionError(f"text {child!r} in a loose item")
        else:
            run.append(child)
    flush()
    return ("item", attrs, result)


def is_loose(items):
    """Return whether the list of ITEMS is loose, its paragraphs shown in <p>.
    A list without paragraphs shows the same either way."""
    return any(not isinstance(child, str) and child[0] == "p"
               for _, _, children in items for child in children)


def block(node):
    tag, attrs, children = node
    if tag == "p":
        return ("paragraph", {}, inlines(children))
    if re.fullmatch(r"h[1-6]", tag):
        return ("heading", {"level": int(tag[1])}, inlines(children))
    if tag == "hr":
        return ("thematic-break", {}, [])
    if tag == "blockquote":
        return ("block-quote", {}, blocks(children))
    if tag in ("ul", "ol"):
        items = [c for c in children if not isinstance(c, str)]
        if any(c[0] != "li" for c in items):
            raise ConversionError(f"<{tag}> holds other than <li>")
        tight = not is_loose(items)
        list_attrs = {"ordered": tag == "ol"}
        if tag == "ol":
            list_attrs["start"] = int(attrs.get("start", "1"))
        return ("list", list_attrs, [item(i, tight) for i in items])
    if tag == "pre":
        if len(children) != 1 or isinstance(children[0], str) or children[0][0] != "code":
            raise ConversionError("<pre> without a single <code>")
        _, code_attrs, code = children[0]
        language = code_attrs.get("class", "").removeprefix("language-") or None
        return ("code-block", {"language": language}, [text_of(code)] if code else [])
    if tag == "table":
        rows = []
        for part in blocks_of(children):
            header = part[0] == "thead"
            for row in blocks_of(part[2]):
                rows.append(("table-row", {"header": header},
                             [("table-cell", {"align": cell[1].get("align")},
                               inlines(cell[2]))
                              for cell in blocks_of(row[2])]))
        return ("table", {}, rows)
    raise ConversionError(f"<{tag}> where a block is expected")


def blocks_of(children):
    """Return the elements of CHILDREN, which may only add newlines."""
    for child in children:
        if isinstance(child, str) and child.strip("\n"):
            raise ConversionError(f"text {child!r} in a table")
    return [c for c in children if not isinstance(c, str)]


def inline(node):
    tag, attrs, children = node
    if tag == "em":
        return ("emphasis", {}, inlines(children))
    if tag == "strong":
        return ("strong", {}, inlines(children))
    if tag == "del":
        return ("strikethrough", {}, inlines(children))
    if tag == "code":
        return ("code", {}, [text_of(children)])
    if tag == "br":
        return ("hard-break", {}, [])
    if tag == "a":
        return ("link", {"destination": attrs.get("href", ""),
                         "title": attrs.get("title")}, inlines(children))
    if tag == "img":
        alt = attrs.get("alt", "")
        return ("image", {"destination": attrs.get("src", ""),
                          "title": attrs.get("title")}, [alt] if alt else [])
    raise ConversionError(f"<{tag}> where an inline is expected")


def document(html):
    _, _, children = DomBuilder().tree(html)
    return ("document", {}, blocks(children))


def lisp(value):
    """Return VALUE written as Lisp."""
    if value is None or value is False:
        return "nil"
    if value is True:
        return "t"
    if isinstance(value, int):
        return str(value)
    if isinstance(value, str):
        return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'
    if isinstance(value, dict):
        return "(" + " ".join(f":{k} {lisp(v)}" for k, v in value.items()) + ")"
    tag, attrs, children = value
    return "(" + " ".join([tag, lisp(attrs)] + [lisp(c) for c in children]) + ")"


def commonmark_examples(path):
    with open(path, encoding="utf-8") as f:
        for e in json.load(f):
            if (e["section"] in RAW_HTML_SECTIONS
                    or e["example"] in RAW_HTML_EXAMPLES | AUTOLINKED_EXAMPLES):
                continue
            yield ("commonmark", e["example"], e["section"], e["markdown"], e["html"])


def gfm_examples(path):
    with open(path, encoding="utf-8") as f:
        spec = f.read()
    section = None
    number = 0
    example = re.compile(r"^`{32} example(?: ([a-z]+))?\n(.*?)^\.\n(.*?)^`{32}$",
                         re.S | re.M)
    heading = re.compile(r"^#+ (.*)$", re.M)
    pos = 0
    while (match := example.search(spec, pos)):
        for h in heading.finditer(spec, pos, match.start()):
            section = h.group(1)
        number += 1
        pos = match.end()
        if match.group(1) in GFM_EXTENSIONS:
            yield ("gfm", number, section,
                   match.group(2).replace("→", "\t"), match.group(3).replace("→", "\t"))


def cjk_friendly_examples(path):
    with open(path, encoding="utf-8") as f:
        for number, e in enumerate(json.load(f), 1):
            yield ("cjk-friendly", number, e["file"], e["markdown"], e["html"])


def convert(commonmark, gfm, cjk_friendly):
    """Return the test cases of COMMONMARK, GFM and CJK-FRIENDLY as Lisp."""
    lines = [";;; expected.eld --- generated by convert.py; do not edit  -*- lexical-binding: t; -*-",
             "("]
    for source, number, section, markdown, html in [*commonmark_examples(commonmark),
                                                     *gfm_examples(gfm),
                                                     *cjk_friendly_examples(cjk_friendly)]:
        try:
            tree = document(html)
        except ConversionError as error:
            raise ConversionError(f"{source} example {number}: {error}") from None
        lines.append(f" (:source {source} :example {number} :section {lisp(section)}"
                     f"\n  :markdown {lisp(markdown)}\n  :expected {lisp(tree)})")
    lines.append(")")
    return "\n".join(lines) + "\n"


if __name__ == "__main__":
    sys.stdout.write(convert(sys.argv[1], sys.argv[2], sys.argv[3]))
