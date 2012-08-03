from monads import reader_m
from collections import namedtuple

class Logger:
    alllogs = []
    def __init__(self):
        self.lines = []
        Logger.alllogs.append(self)

    def log(self, line): self.lines.append(line)


class Servlet:
    def __init__(self, logger, settings):
        self.logger = logger
        self.settings = settings

    def log(self, line):
        self.logger.log(line)

    def post(self, request, db):
        self.log("post: %s"%request)

    def get(self, request, db):
        self.log("get: %s"%request)

class PageServlet(Servlet): pass
class DatastoreServlet(Servlet): pass


Env = namedtuple('Env', ['logger', 'settings', 'servletClasses'])
# getters describe how to read a value from an environment,
# but do not have access to the environment itself
getLogger = lambda env: env.logger
getSettings = lambda env: env.settings
getServletClasses = lambda env: env.servletClasses


def bootstrap():
    """no dependencies passed as parameters. could have very long list of
    dependencies"""
    r = reader_m.bind( getServletClasses, lambda servlets:
        reader_m.bind( getLogger, lambda Logger:
        reader_m.bind( getSettings,  lambda settings:
        reader_m.unit( [ Servlet(Logger(), settings) for Servlet in servlets] ))))
    return r

def main():
    env = Env(Logger, {'debug': True}, [PageServlet, DatastoreServlet])
    servlets = bootstrap()(env)

    [servlet.get({'foo':'bar'}, None) for servlet in servlets]

    print [log.lines for log in Logger.alllogs]

main()


def bootstrap2(env):
    return [Servlet(env.logger(), env.settings) for Servlet in env.servletClasses]

def main2():
    env = Env(Logger, {'debug': True}, [PageServlet, DatastoreServlet])
    servlets = bootstrap2(env)

    [servlet.get({'foo':'bar'}, None) for servlet in servlets]

    print [log.lines for log in Logger.alllogs]

main2()
