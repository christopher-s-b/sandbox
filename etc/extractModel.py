from __future__ import print_function

model = [
    'param1',
    'param2',
     [
        'foo',
        'bar'
     ],
    'missingParam'
]

request = {'param1':"hello", 'param2':42, 'foo':'foofoo', 'bar':'barbar' }

def traverse(visit, tree):
    level = [node for node in tree]
    fields = filter(lambda x: type(x) != type([]), level)
    subModels = filter(lambda x: type(x) == type([]), level)

    acc = [traverse(visit, node) for node in subModels]
    obj = visit(fields)
    if acc != []: obj['children'] = acc

    return obj
    

def visit(fields):
    relevantRequest = filter(lambda item: item in fields, request)
    instance = {}
    for param in relevantRequest:
        instance[param] = request[param]
    return instance

def process(request, model):
    return traverse(visit, model)
    
print(process(request, model))

