Pickle 
======
*a delightful little markup language*

What, another one?
------------------

Pickle is a markup language, and in the current technical landscape creating yet another markup language requires some justification. So, before we get to what Pickle markup looks like,
let's talk a bit about why it exists.

Fundamentally, the purpose of markup is to attach semantic information to text - to give bits of text meaning that can be used by computers. There are lots of markup languages around, but 
unfortunately I haven't been able to find any that I think are actually very good at solving this fundamental problem. There are currently a number of reasonably good solutions for encoding
structured data in text (such as JSON, YAML and their bretheren); many that are helpful for writing documentation that can be readily turned into simple HTML, like the markdown syntax
in which I'm currently writing this document, but when it comes to actually attaching semantic information to text, the options dwindle down to a very small set.

In my opinion, the most important thing that a markup language needs to do is to provide a clear and consistent separation between data and metadata. Some markup languages like XML and TeX
are relatively good at this (when used correctly) but in both of those cases, there's a major problem: metadata is not first-class.

First Class Metadata
--------------------

In XML, tag bodies constitute data, and attribute/value pairs serve for metadata. For simple markup with limited semantic complexity, this is workable, but as soon as you start trying to
represent any sort of sophisticated data in XML, this limitation begins to chafe, and for a very obvious reason; metadata is itself data that may have internal structure that simple 
key/value pairs simply aren't up to the task of representing. Furthermore, XML is excessively verbose when it comes to tagging simple values; frequently, the amount of markup exceeds the
amount of data by a large margin. As a consequence, there is a history of people using XML tag attributes to store data instead of metadata, simply to avoid having to include closing tags
in the markup. This practice is so widespread that in many cases it makes the distinction between data and metadata that XML provides almost meaningless. 

Pickle is an attempt to address these problems.

Syntax
======

A tag in Pickle is introduced with the @ symbol. There are two tag forms, a long form and a short form.

Short Form
----------

Short Pickle tags have the form:

    @tagname[tagdata | metadata]

*tagname* is an opaque string identifying the semantic domain. There are very few restrictions on the characters that can be used in a tag name; it cannot contain whitespace, and the 
characters '@', '[' and ']' (as well as control characters) are reserved, but any other character is accepted.

*tagdata* is arbitrary Pickle data, which can contain text and markup. Literal '|' characters in text (that is not itself nested in a tag) can be escaped with a backslash.

*metadata* is a space-separated list of short Pickle tags.

Long Form
---------

Long Pickle tags can take two forms which differ only in the way that they are closed.

    @[tagnamedata | metadata]
      tagdata
    @/

or

    @[tagname | metadata]
      tagdata
    @[/tagname]

The former is generally preferred, but the latter is accepted mostly for the purposes where it's handy to document the end of a long marked-up block as corresponding to a particular opening tag.

Here, the components of the tag have the same meanings as with the short form, with the exception of the *tagnamedata* field of the first form, which is termed the short-close long form. In the short-close long form, the limitation on tag name format is lifted, and any valid Pickle document can serve as the tag identifier. The reason for this will be clarified later when we talk about tag references.
