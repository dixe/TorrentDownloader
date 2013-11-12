import urllib2
import sys

file("html.txt", "w").write(urllib2.urlopen(sys.argv[1]).read())