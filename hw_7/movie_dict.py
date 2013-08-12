import json
import os

#path = 'MOVIEDATA/movie_dict.json'

flag = 0
try: 
	json.loads(open(path.read())
except: 
	flag =1

#if not os.path.exists(path):
if flag=1:
	out = {}
	data=open('movies.dat').read()
	data = data.split('\n')
	for d in data:
		d = d.split('::')
		try: 
			tmp = str(d[0])
			if tmp not in out:
				out[tmp]=d[1].encode('utf8')
		except:pass
	print out[str(1)]
	out = json.dumps(out)
	f = open('movie_dict.json', 'w')
	f.write('%s' % str(out))
	f.close()
else:
	print 'file already exists'
