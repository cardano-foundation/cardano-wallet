# **Document Storage :: AP**

|         |            |
|---------|------------|
| Started | 2022-09-30 |
| Decided | 2022-10-14 |

## **Why**

We need to store our documents — such as decision records, design documents — somewhere, but there are a plethora of options: Google Docs, Confluence, Github, Custom wiki, … with various pros and cons. This decision records our current choice and its rationale.

## **Decision**

The entry point to our documentation is the Adrestia (Haskell) Team Dashboard on Google Docs. All other documents are hyperlinked from this dashboard.

For documents that are not public, such as meeting minutes or Advice Process records, we use Google Docs.

For documents that may eventually be public, such as design documentation or release checklists, we use Rodney’s HedgeDoc instance at [https://md.adrestia.iohkdev.io/](https://md.adrestia.iohkdev.io/) . (As Rodney is no longer with us, I’m actually not happy with that state of affairs. Moritz Angerman has set up another instance at [https://hd.devx.iog.io/](https://hd.devx.iog.io/) ).

## **Details**

See Section “Decision”.

## **Rationale**

**Making** a **decision** — Document storage systems come with various trade-offs, and none seems to be perfect. The act of simply picking one system is at least as important as making sure that the pick is reasonable — we can’t even record our choice unless we have already made it\! Thus, I (the decision maker) have decided to favor a speedy decision over an optimal one.

What do we want out of a document storage system? Here is a list of features, in order of importance, and some thoughts as to how these are satisfied with my current decision.

**Sharing** and **Access Control** — We need to be able to share documents with team members, and maybe external parties. Also, we need to keep some types of documents internal to the company or team.

**Readability** — Documents should be pleasant on the eyes. For me, this means that they should have a rendered view, but they do not need to be WSIWYG.

**Commenting** and **Tracking Changes** — It should be pleasant to comment on documents and track changes that different contributors made. For me, this means that it should be possible to comment on the *rendered* view. For example, markdown documents in a Github repository do not satisfy this property, as I can only see changes in the source code, and comments are also attached to the source code rather than the rendering.

**Writing format** — Documents should be easy to export to Markdown, so that we can more easily move them between different storage solutions, e.g. from HedgeDoc to our Github repositories. Unfortunately, Google Docs does not allow easy export to Markdown; this is particularly painful for documents with a lot of source code — hence we use HedgeDoc for more technical documents.

**Navigation** — Documents should be easy to **search** (“I know what I’m looking for, but I cannot find it.”) and to **discover** (“I don’t know what I’m looking for, but I find it anyway”).

From my experience, good discoverability comes from good curation. In my view, Hyperlinks are the best tool for curating document listings. For example, Confluence has a hierarchical view for each space, but I still find it hard to navigate if the sections are not curated properly. The [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell) uses ToC templates and manually curated tables of hyperlinks, I think it works very well.

For searching a document, curation helps, too, but a search box is usually more effective.
