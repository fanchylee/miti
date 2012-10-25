
for i in range(0x4E00,0xA000):
	u=unichr(i)
	ch=u.encode('utf-8')
	print('(' + hex(i)[2:].upper() + ' ' + ch + ')')

