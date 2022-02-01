1. https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide
2. https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide
https://macromates.com/manual/en/language_grammars#naming_conventions
https://macromates.com/manual/en/regular_expressions
https://dev.to/ronsoak/i-built-my-own-vs-code-syntax-highlighter-from-scratch-and-here-s-what-i-learned-1h98
https://github.com/microsoft/vscode-textmate/blob/main/test-cases/first-mate/fixtures/c.json
https://code.visualstudio.com/api/working-with-extensions/publishing-extension


local packaging:
``vsce package`` generate the ``aspect-0.0.1.vsix``
then manual install ``code --install-extension aspeclg-0.0.1.vsix``
then restart vscode

Ctrl Alt Maj i => Developper token inspector