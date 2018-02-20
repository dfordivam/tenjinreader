## Build artifacts

### Server
- Nix build path

### FrontEnd
  frontend.jsexe -> complete-norecog
  frontend-srs-only.jsexe -> srsonly-norecog
  frontend-recog.jsexe -> complete
  frontend-reader-only.jsexe -> readeronly
  frontend-srs-only-recog.jsexe -> srsonly

## Static Data

- Credentials - Github, Twitter, Google - ClientId, Client secret
- Binary Data
  - kanjidb.bin
  - sentencedb.bin
  - booksdb.bin
  - articlesdb.bin -- Not used right now
  
- static/
  - wanakana.min.js
  - runmain.js


## Runtime artifacts

- userdb.sqlite3
- userData/
- config/

## External Dependencies

- mecab - executable
- mecab-files-dir

## Deploy Dependencies

- closure-compiler - "/nix/store/n7h36mmdgd9y5g59qasznjvsl3psg0g2-closure-compiler-20170218"
- deploy/
  - index.html.template
