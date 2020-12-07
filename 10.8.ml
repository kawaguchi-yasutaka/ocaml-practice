#use "ketsueki_shukei.ml" 

let saita_ketsueki lst = let (a,b,o,ab) = ketsueki_shukei lst in 
  if a > b && a > 0 && a > ab then "A"
  else if b > a && b > o && b > ab then "B"  
  else if o >a && o > b && o > ab then "O"
  else if ab > a && ab > b && ab > o then "AB"
  else "unknown"

let test_saita_ketsueki1 = saita_ketsueki [
  {name = "川口";blood_type = "A"};
  {name = "川口";blood_type = "A"};
  {name = "川口";blood_type = "O"};
  {name = "川口";blood_type = "AB";
  }] = "A"
