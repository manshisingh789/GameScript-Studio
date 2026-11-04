module GUI.CompilationStatsPanel
    ( CompilationStatsPanel(..)
    , setupCompilationStatsPanel
    , updateCompilationStatsPanel
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Statistics.CompilationStats
import Control.Monad (void)

data CompilationStatsPanel = CompilationStatsPanel
    { cspContainer :: UI.Element
    , cspLines     :: UI.Element
    , cspTokens    :: UI.Element
    , cspAST       :: UI.Element
    , cspBytecode  :: UI.Element
    , cspTime      :: UI.Element
    }

setupCompilationStatsPanel :: UI.UI CompilationStatsPanel
setupCompilationStatsPanel = do
    title <- UI.h3 #+ [UI.string "Compilation Statistics"]

    linesValue <- UI.span # set UI.text "N/A"
    tokensValue <- UI.span # set UI.text "N/A"
    astValue <- UI.span # set UI.text "N/A"
    bytecodeValue <- UI.span # set UI.text "N/A"
    timeValue <- UI.span # set UI.text "N/A"

    let metric label value =
            UI.div #. "stat-item"
                #+ [ UI.span #. "stat-label" # set UI.text label
                   , element value
                   ]

    linesRow <- metric "Logical lines of code: " linesValue
    tokensRow <- metric "Tokens: " tokensValue
    astRow <- metric "AST Nodes: " astValue
    bytecodeRow <- metric "Bytecode Instructions: " bytecodeValue
    timeRow <- metric "Compile Time: " timeValue

    container <- UI.div
        #. "compilation-stats"
        #+ [ element title
           , element linesRow
           , element tokensRow
           , element astRow
           , element bytecodeRow
           , element timeRow
           ]

    return CompilationStatsPanel
        { cspContainer = container
        , cspLines = linesValue
        , cspTokens = tokensValue
        , cspAST = astValue
        , cspBytecode = bytecodeValue
        , cspTime = timeValue
        }

updateCompilationStatsPanel
    :: CompilationStatsPanel
    -> CompilationStats
    -> UI.UI ()
updateCompilationStatsPanel panel stats = do
    void $ element (cspLines panel) # set UI.text
        (show (statLogicalLines stats))

    void $ element (cspTokens panel) # set UI.text
        (show (statTokenCount stats))

    void $ element (cspAST panel) # set UI.text
        (maybe "N/A" show (statASTNodes stats))

    void $ element (cspBytecode panel) # set UI.text
        (maybe "N/A" show (statBytecodeCount stats))

    void $ element (cspTime panel) # set UI.text
        (show (fromIntegral (round (statCompileTimeMs stats * 100)) / (100 :: Double)) ++ " ms")