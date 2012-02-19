import SocketServer
import BaseHTTPServer
import SimpleHTTPServer
from time import sleep

class ThreadingHTTPServer(SocketServer.ThreadingMixIn, BaseHTTPServer.HTTPServer):
    pass

class Handler(SimpleHTTPServer.SimpleHTTPRequestHandler):
    def do_GET(self):
        sleep(.5)
        self.wfile.write("hello world\n")

address = ("", 8010)
httpd = ThreadingHTTPServer(address, Handler)
httpd.serve_forever()


