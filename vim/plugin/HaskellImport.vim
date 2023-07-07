
function HaskellImport(name)
    :normal G
    let modulelineNumber = search('module')
    if modulelineNumber == 0
        let lineNumber = 0
    endif
    let importlineNumber = search('import', 'b')
    if importlineNumber == 0
        let lineNumber = modulelineNumber
    else
        let lineNumber = importlineNumber
    endif
    let baseline = 'import qualified '
    if a:name == 'list'
        let specline = 'Data.List as List'
    elseif a:name == 'map'
        let specline = 'Data.Map as Map'
    elseif a:name == 'set'
        let specline = 'Data.Set as Set'
    elseif a:name == 'vector'
        let specline = 'Data.Vector as Vector'
    elseif a:name == 'matrix'
        let specline = 'Data.Matrix as Matrix'
    elseif a:name == 'protolude'
        let specline = 'Protolude'
    else
        echom "Wrong arguments to HaskellInsert()"
        return
    endif
    let line = baseline . specline
    let failed = append(lineNumber, line)
endfunction
