# VCard

An implementation of:

* [RFC6350: vCard Format Specification](https://datatracker.ietf.org/doc/html/rfc6350)
* [RFC6868: Parameter Value Encoding in iCalendar and vCard](https://datatracker.ietf.org/doc/html/rfc6868)

## Status

Not ready yet.

## Goals

* Strict RFC Compliance
* Being able to deal with some of other implementers' failures to comply with the spec

## Implementation checklist

### RFC 6350

- [ ] 1.  Introduction
- [ ] 2.  Conventions 
- [ ] 3.  vCard Format Specification
  - [ ] 3.1.  Charset 
  - [ ] 3.2.  Line Delimiting and Folding 
  - [ ] 3.3.  ABNF Format Definition
  - [ ] 3.4.  Property Value Escaping 
- [ ] 4.  Property Value Data Types 
  - [ ] 4.1.  TEXT
  - [ ] 4.2.  URI 
  - [ ] 4.3.  DATE, TIME, DATE-TIME, DATE-AND-OR-TIME, and TIMESTAMP
    - [ ] 4.3.1.  DATE
    - [ ] 4.3.2.  TIME
    - [ ] 4.3.3.  DATE-TIME 
    - [ ] 4.3.4.  DATE-AND-OR-TIME
    - [ ] 4.3.5.  TIMESTAMP 
  - [ ] 4.4.  BOOLEAN 
  - [ ] 4.5.  INTEGER 
  - [ ] 4.6.  FLOAT 
  - [ ] 4.7.  UTC-OFFSET
  - [ ] 4.8.  LANGUAGE-TAG
- [ ] 5.  Property Parameters 
  - [ ] 5.1.  LANGUAGE
  - [ ] 5.2.  VALUE 
  - [ ] 5.3.  PREF
  - [ ] 5.4.  ALTID 
  - [ ] 5.5.  PID 
  - [ ] 5.6.  TYPE
  - [ ] 5.7.  MEDIATYPE 
  - [ ] 5.8.  CALSCALE
  - [ ] 5.9.  SORT-AS 
  - [ ] 5.10. GEO 
  - [ ] 5.11. TZ
- [ ] 6.  vCard Properties
  - [ ] 6.1.  General Properties
    - [ ] 6.1.1.  BEGIN 
    - [ ] 6.1.2.  END 
    - [ ] 6.1.3.  SOURCE
    - [ ] 6.1.4.  KIND
    - [ ] 6.1.5.  XML 
  - [ ] 6.2.  Identification Properties 
    - [ ] 6.2.1.  FN
    - [ ] 6.2.2.  N 
    - [ ] 6.2.3.  NICKNAME
    - [ ] 6.2.4.  PHOTO 
    - [ ] 6.2.5.  BDAY
    - [ ] 6.2.6.  ANNIVERSARY 
    - [ ] 6.2.7.  GENDER
  - [ ] 6.3.  Delivery Addressing Properties
    - [ ] 6.3.1.  ADR 
  - [ ] 6.4.  Communications Properties 
    - [ ] 6.4.1.  TEL 
    - [ ] 6.4.2.  EMAIL 
    - [ ] 6.4.3.  IMPP
    - [ ] 6.4.4.  LANG
  - [ ] 6.5.  Geographical Properties 
    - [ ] 6.5.1.  TZ
    - [ ] 6.5.2.  GEO 
  - [ ] 6.6.  Organizational Properties 
    - [ ] 6.6.1.  TITLE 
    - [ ] 6.6.2.  ROLE
    - [ ] 6.6.3.  LOGO
    - [ ] 6.6.4.  ORG 
    - [ ] 6.6.5.  MEMBER
    - [ ] 6.6.6.  RELATED 
  - [ ] 6.7.  Explanatory Properties
    - [ ] 6.7.1.  CATEGORIES
    - [ ] 6.7.2.  NOTE
    - [ ] 6.7.3.  PRODID
    - [ ] 6.7.4.  REV 
    - [ ] 6.7.5.  SOUND 
    - [ ] 6.7.6.  UID 
    - [ ] 6.7.7.  CLIENTPIDMAP
    - [ ] 6.7.8.  URL 
    - [ ] 6.7.9.  VERSION 
  - [ ] 6.8.  Security Properties 
    - [ ] 6.8.1.  KEY 
  - [ ] 6.9.  Calendar Properties 
    - [ ] 6.9.1.  FBURL 
    - [ ] 6.9.2.  CALADRURI 
    - [ ] 6.9.3.  CALURI
  - [ ] 6.10. Extended Properties and Parameters
- [ ] 7.  Synchronization 
  - [ ] 7.1.  Mechanisms
    - [ ] 7.1.1.  Matching vCard Instances
    - [ ] 7.1.2.  Matching Property Instances 
    - [ ] 7.1.3.  PID Matching
  - [ ] 7.2.  Example 
    - [ ] 7.2.1.  Creation
    - [ ] 7.2.2.  Initial Sharing 
    - [ ] 7.2.3.  Adding and Sharing a Property 
    - [ ] 7.2.4.  Simultaneous Editing
    - [ ] 7.2.5.  Global Context Simplification 
- [ ] 8.  Example: Author's vCard 
- [ ] 9.  Security Considerations 
- [ ] 10. IANA Considerations 
  - [ ] 10.1. Media Type Registration 
  - [ ] 10.2. Registering New vCard Elements
    - [ ] 10.2.1. Registration Procedure
    - [ ] 10.2.2. Vendor Namespace
    - [ ] 10.2.3. Registration Template for Properties
    - [ ] 10.2.4. Registration Template for Parameters
    - [ ] 10.2.5. Registration Template for Value Data Types
    - [ ] 10.2.6. Registration Template for Values
  - [ ] 10.3. Initial vCard Elements Registries 
    - [ ] 10.3.1. Properties Registry 
    - [ ] 10.3.2. Parameters Registry 
    - [ ] 10.3.3. Value Data Types Registry 
    - [ ] 10.3.4. Values Registries 
- [ ] 11. Acknowledgments 
- [ ] 12. References
  - [ ] 12.1. Normative References
  - [ ] 12.2. Informative References
- [ ] Appendix A.  Differences from RFCs 2425 and 2426
  - [ ] A.1.  New Structure 
  - [ ] A.2.  Removed Features
  - [ ] A.3.  New Properties and Parameters 

#### [Errata](https://www.rfc-editor.org/errata/rfc6350)


- [ ] [3086](https://www.rfc-editor.org/errata/eid3086)
- [ ] [3136](https://www.rfc-editor.org/errata/eid3136)
- [ ] [3377](https://www.rfc-editor.org/errata/eid3377)
- [ ] [3484](https://www.rfc-editor.org/errata/eid3484)
- [ ] [3713](https://www.rfc-editor.org/errata/eid3713)
- [ ] [3846](https://www.rfc-editor.org/errata/eid3846)
- [ ] [3845](https://www.rfc-editor.org/errata/eid3845)
- [ ] [2964](https://www.rfc-editor.org/errata/eid2964)
- [ ] [3000](https://www.rfc-editor.org/errata/eid3000)
- [ ] [3137](https://www.rfc-editor.org/errata/eid3137)
- [ ] [3368](https://www.rfc-editor.org/errata/eid3368)
- [ ] [3748](https://www.rfc-editor.org/errata/eid3748)
- [ ] [4246](https://www.rfc-editor.org/errata/eid4246)
- [ ] [4246](https://www.rfc-editor.org/errata/eid4246)


TODO unverified errata 

### RFC 6868

- [ ] 1. Introduction
- [ ] 2. Conventions Used in This Document
- [ ] 3. Parameter Value Encoding Scheme
    - [ ] 3.1. iCalendar Example
    - [ ] 3.2. vCard Example
- [ ] 4. Security Considerations
- [ ] 5. Acknowledgments
- [ ] 6. Normative References
- [ ] Appendix A. Choice of Quoting Mechanism

#### [Errata](https://www.rfc-editor.org/errata/rfc6868)

- [ ] [4383](https://www.rfc-editor.org/errata/eid4383)
