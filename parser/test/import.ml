include struct
  open Holo_lexer
  module Lexer = Lexer
  module Regex_config = Regex_dfa.Config
  module Regex_dfa = Regex_dfa
  module Source_position = Source_position
  module Token = Token
  module With_errors = With_errors
end

include struct
  open Holo_parser
  module Ast = Ast
  module Expr_parser = Expr_parser
  module Parser = Parser
end
