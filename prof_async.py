import gevent
import sys
from gevent import monkey

monkey.patch_all()

#import urllib2
import requests

def main():
  assert len(sys.argv) >= 3, "bad args: url, count"
  
  url = sys.argv[1]
  count = int(sys.argv[2])
  
  creds=(sys.argv[3], sys.argv[4]) if len(sys.argv)==5 else (None, None)

  urls = [url]*count
  jobs = [gevent.spawn(download, url, creds) for url in urls]

  gevent.joinall(jobs, timeout=20)
  
def download(url, creds):
  r=requests.get(url, auth=creds)
  r.content
  print "%s %s"%(r.status_code, url)

if __name__ == "__main__":
  main()
