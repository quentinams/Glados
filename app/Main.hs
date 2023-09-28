import Datas
import Evaluation

main :: IO ()
main = do
    print $ evalExpr (List [Symbol "+", Number 10, List[Symbol "/", Number 6, Number 2]])
    print $ evalExpr (List [Symbol "eq?", Number 10, Number 10])
    print $ evalExpr (List [Symbol ">", Number 8, Number 10])