def shrink_error(num):
	i = 0
	while float('{:.{prec:d}f}'.format(num, prec=i)) / num < 0.8: i += 1
	return '{:.{prec:d}f}'.format(num, prec=i), i

def fmt(num, error):
	error, digits = shrink_error(error)
	return '{:.{prec:d}f}±{:s}'.format(num, error, prec=digits)
