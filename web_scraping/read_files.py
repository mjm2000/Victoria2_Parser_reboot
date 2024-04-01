from bs4 import BeautifulSoup
soup = BeautifulSoup(open("./List_of_conditions"),"html.parser")

print("exception Syntax_error of string")
print("open Types")
print("let country_conditions lexems out =")
print("let rec condition_r lexems out= match lexems with")
for x in soup.find_all('pre'):
    tokens = x.text.replace(' ','').split('=')
    type = "keyword"

    if tokens[0] == "trigger":continue
    
    match tokens[1]:
        case 'x':
            type = 'int' 
        case 'X':
            type = 'float' 
        case '[yes/no]':
            type = 'bool'
        case '{':
            type = 'LB'
        case x if len(tokens)>2:
            type = ""

    print('|(KEYWORD,(\"{}\"),_)::(EQ,_,_)::({},value,_)::rest->'.format(tokens[0],type.upper()) )
    match type:
        case 'keyword' | "" :
            conv = ""
        case _:
            conv = "{}_of_string".format(type)
    print("    let v = {}({}(value)) in".format(tokens[0].upper(),conv))
    print("    condition_r rest (v::out)") 
    print("|(KEYWORD,(\"{}\"),_)::(EQ,_,_)::(wrong_type,value,pos)::rest->".format(tokens[0]))
    print("    let v = CONDITION_TYPE_ERROR [wrong_type,value,pos] [{}]".format(type.upper()))
    print("    condition_r rest (v::out)")
    if tokens[0] == 'war_with':
        print("|RB::rest->out,rest")
        print("|_->throw Syntax_error \"wrong type\" ")
        print("\nin\ncondition_r lexems []")
        print("let province_conditions lexems out= ")
        print("let rec condition_r lexems out= match lexems with")

print("|RB::rest->out,rest")
print("|_->raise (Syntax_error \"wrong type\") ")
print("\nin\ncondition_r lexems []")
