import os,sys

loaded = {}

filelist = [open(sys.argv[1])]
pathlist = ["./"]
in_comment = 0
headers = ""
while len(filelist):
  l = filelist[-1].readline()
  if l=="":
    filelist.pop()
    pathlist.pop()
    continue
  if ("#include" in l) and (not "MQ2" in l):
	if "<" in l:
	  headers = headers + "\n" + l.strip()
	  print l.strip()
	  continue
	if not '"' in l:
	  print l.strip()
	  continue
	l = l.strip()
	l = l[l.find('"')+1:-1]
	path = l.rsplit("/",1)
	if len(path) == 2:
	  pathlist.append(pathlist[-1]+path[0]+"/")
	  path[0] = path[1]
	else:
	  pathlist.append(pathlist[-1])
	if not path[0] in loaded:
	  filelist.append(open(pathlist[-1]+path[0]))
	  loaded[path[0]] = 1
	else:
	  pathlist.pop()
  else:
	l = l.strip()
	if l == "/*":
	  in_comment = 1
	  l = ""
	if (in_comment == 1) and l.find("*/") != -1:
	  in_comment = 0
	  l = l[l.find("*/")+2:]
	if l.find("/*") != -1 and l.find("*/") != -1:
	  st = l.find("/*")
	  en = l.find("*/")+2
	  l = l[0:st]+l[en:]
	if len(l) and (in_comment == 0):
	  print l
print "/*"
print headers
print "*/"