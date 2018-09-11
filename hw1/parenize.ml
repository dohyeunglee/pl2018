(*
 * 2015-11531, 이도형
 *)


type team =
  | Korea
  | France
  | Usa
  | Brazil
  | Japan
  | Nigeria
  | Cameroon
  | Poland
  | Portugal
  | Italy
  | Germany
  | Norway
  | Sweden
  | England
  | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let team_to_string =
  function
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"

let rec parenize =
  function
  | LEAF team -> team_to_string team
  | NODE (left, right) -> "(" ^ parenize left ^ " " ^ parenize right ^ ")"

