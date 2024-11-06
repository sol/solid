syn match solidUseGroup "^\<use\>.*" contains=solidUse,hsImportModuleName,solidUseMod,hsLineComment,hsBlockComment,hsImportList,@NoSpell nextgroup=solidUse

syn keyword solidUse use contained nextgroup=hsImportModuleName
hi def link solidUse hsImport

syn keyword solidUseMod contained as hiding with
hi def link solidUseMod solidUse
