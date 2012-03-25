
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
    return {k:v for k,v in request if k in fields}
#    relevantRequestParams = filter(lambda item: item in fields, request)
#    print relevantRequestParams, request
#    return dict(map(lambda param: (param, request[param]), relevantRequestParams))

def process(request, model):
    return traverse(visit, model)

assert process(request, model) == {'param2': 42, 'param1': 'hello', 'children': [{'foo': 'foofoo', 'bar': 'barbar'}]}    

