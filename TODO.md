## Known troubleshootings

## Setup/Working environment
* Setup CI
* Setup logging functionalities
* Unify error handling

## Code structure
* Load plugin from target files

## Src 
* Propagate place annotation until codegeneration to be able to print meaningfull error/warning.
  * Get ride of place during the generation the Akka (Rt Ast)

### Akka plugin
* Translate session from IR to Akka
* Translate channels 

### Java plugin
* Handle indentation when generating the source code
* Optional: Add a cleansing pass before outputing the code

### GADT during parsing
https://arxiv.org/pdf/1807.06702.pdf