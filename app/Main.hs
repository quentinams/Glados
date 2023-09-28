import Datas
import Evaluation

main :: IO ()
main = do
    print $ evalExpr (List [Symbol "mod", Number 117, Number 17])
    print $ evalExpr (List [Symbol "eq?", Number 10, Number 10])
    print $ evalExpr (List [Symbol ">", Number 8, Number 10])