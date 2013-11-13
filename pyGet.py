import urllib2
import sys

body = urllib2.urlopen(sys.argv[1]).read()
file("html.txt", "w").write(body)
