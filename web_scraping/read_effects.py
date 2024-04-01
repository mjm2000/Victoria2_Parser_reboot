from bs4 import BeautifulSoup
import re
soup = BeautifulSoup(open("./List_of_effects"),"html.parser")
#
def get_effects():
    effect_dict = {'Province':[],'POPs':[],'State':[],'Country':[],'Global':[]}
    for x in soup.find_all(class_="wikitable"):
        table = x.tbody.tr.find_all('th')
        header =  [x.text.strip('\n') for x in table] 
        expected_header = ['Name', 'Target', 'Syntax', 'Effect', 'Notes']
        table_rows= x.tbody.find_all('tr')
        if (header == expected_header):
            for table_entry_index in range(1,len(table_rows)): 
                table_entry = table_rows[table_entry_index]
                cat = table_entry.find_all('td')[1].text.strip('\n')
                code = table_entry.find_all('td')[2].text
                #code = re.sub('[\s\t]+','',code)
                arg_types = list(filter(lambda x: x!='',re.split(('\n|\s'),code)))
                if cat in effect_dict and arg_types != []:
                    effect_dict[cat].append((arg_types[0],arg_types[1:]) )
    return effect_dict

def write_type_effects(effects):
    for effect_cat in effects: 
        values = effects[effect_cat]
        print("type {} =".format(effect_cat.lower()))
        for kw in values: 
            print("|{} of ".format(kw[0].upper()),end='')
            match kw[1][1]:
                case 'n':
                    print('int')
                case '[yes/no]':
                    print('bool')
                case '{':
                    #match kw[1][2]
                    match kw[1][3]:
                        case 'n':
                            print('int * ',end='')
                        case '[yes/no]':
                            print('bool * ',end='')
                        case _:
                            print('string * ',end='') 
                    match kw[1][6]:
                        case 'n':
                            print('int')
                        case '[yes/no]':
                            print('bool')
                        case _:
                            print('string')
                case _:
                    print('string') 

def write_functions(effects):
    print("open Types\nopen Effect_type")
    for effect_cat in effects:
        values = effects[effect_cat]  
        print("let {}_effects effects =".format(effect_cat.lower()))
        print("let rec {}_effects_r effects out = match effects with".format(effect_cat.lower()))

        v = ""
        for kw in values:
            print("\t|(KEYWORD,\"{}\",_)".format(kw[0]),end='')

            def print_token(ls):

                global v 
                match ls: 
                    case ['n',*rest]:
                        print('::(INT,v,_)',end='')
                        v = 'int'
                        print_token(rest)
                    case ['yes/no',*rest]:
                        print('::(BOOL,v,_)',end='')
                        v = 'bool'
                        print_token(rest)
                    case ['=',*rest]:
                        print("::(EQ,_,_)",end='')
                        print_token(rest)
                    case ['{',*rest]:
                        print("::(LB,v,_)",end='')
                        print_token(rest)
                    case ['}',*rest]:
                        print("::(LB,v,_)",end='')
                        print_token(rest)
                    case [keyword,*rest]: 
                        v = ""
                        print("::(KEYWORD,\"{}\",_)".format(keyword),end="")
                        print_token(rest)
                    case []:
                       print("::rest->") 
                       value_type = ''
                       match v:
                           case '': value_type = 'v'
                           case type: value_type = "({}_of_string v)".format(v)
                            
                       print("\t\t{}_effects_r rest ({}({}))::out ".format(effect_cat.lower(),kw[0].upper(),value_type))

            print_token(kw[1])
        print("|_->out")
        print("in\n{}_effects_r effects []".format(effect_cat.lower()))            
            
            

effects = get_effects()
write_functions(effects)
#write_type_effects(effects)
