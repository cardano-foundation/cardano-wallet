# **Store Documents alongside Code**

|         |             |
|---------|-------------|
| Started | 13 Nov 2024 |
| Decided | 3 Dec 2024  |

## **Why**

When developing open-source projects, it helps if all the core “artifacts” related to the project are easily accessible along with the code:

- Links between various artifacts, between code and documents, or documents and code, are easier to add, verify, and maintain over time
- Versioning everything in the same way increases the likelihood of maintaining consistency over time
- It reduces “friction” as one only needs to check-out the project and look in some folder to find the needed information
- Documents can be used as build resources to produce e.g. websites or PDFs, and conversely up-to-date code can be included in documents if needed
- Wannabe contributors have all the needed context to contribute more effectively

## **Decision**

Store all documents related to the team's *product* (roadmap, design documents, specifications) and *process* (Advice Process decisions, Logbook, Meeting minutes and recordings, etc.) to the relevant repository (ie. usually https://github.com/cardano-foundation/cardano-wallet).

## **Details**

* This Advice Process supersedes [Document Storage](2022-10-04-document-storage.md)
* The documents shall be moved manually and converted to markdown as needed, i.e. we won't waste time migrating everything all at once and might keep some links to Google docs active for some time
* New documents shall be created in the relevant repository
* Folder structure is not fixed in this advice, and will be defined and refined as needed. Tentative structure could be:
  * docs/ : toplevel documentation directory
  * docs/process : advice process directory
  * docs/process/README.md: contains the Advice Process logbook and summary
  * docs/specifications: specification documents
* The team maintains several code repositories but for the time being, we still consider cardano-wallet to be our main focus, therefore more general documents related to process or products should go there.
* Private documents will be kept in Google Docs
* Public documents that require some collaborative writing or editing process might live for a while in Google Docs or [https://hackmd.io](https://hackmd.io). The latter should be preferred as it ensures we stick to markdown format.

## **Rationale**

The “Why” section already states a few benefits of keeping documents in the same place as the code the team is responsible for. The main rationale behind this proposal is to increase the visibility of the open source Cardano community on the work the HAL team is doing, in order to maximise the likelihood this work will be deemed relevant and important, increase or lower the barrier of entry for potential contribution, and more generally increase the level of transparency of our work.

The original Advice Process’ rationale stated some requirements for documents storage which were valid and this proposal tries to stick with those:

- **Sharing** and **Access Control**: There are very few documents we need to keep private and by default, everything related to the product and process should be public, so no need for specific access control policies
- **Readability**: GH is able to render sophisticated document, and if needed it's possible to enrich raw markdown to produce web sites or PDFs
- **Commenting** and **Tracking Changes**: it's possible and easy to comment through change proposals. While it's not possible to comment directly on rendered document, I don't think this is a showstopper as the documents we manage are not so sophisticated the rendering is completely decorrelated from the source
- **Writing format**: markdown is very versatile and easy to work with. Should we need a WYSIWYG collaborative editor, it's always possible to work in a [https://hackmd.io](https://hackmd.io) document, even in private, and commit the result
- **Navigation**
  - **Search**: repository is indexed and easily searchable through GH interface or directly on the filesystem
  - **Discover**: ditto
