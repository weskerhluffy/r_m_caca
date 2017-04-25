import sys
if __name__ == "__main__":
	lineas = list(sys.stdin)
        nums=[int(x) for x in lineas[1].strip().split(" ")]
#	print("caca %s"%(nums))       
	for linea in lineas[2:]:
		que=[int(x) for x in linea.strip().split(" ")]
#	        print("query act %s"%(que))       
                mini=min(nums[que[0]:que[1]+1])
		print("%s"%mini)
