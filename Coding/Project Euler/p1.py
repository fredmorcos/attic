def f(x,count=False):
    if count==False: return f(x-1,True)
    if x==0: return 0
    elif x%3==0 or x%5==0: return x+f(x-1,True)
    else: return f(x-1,True)

if __name__ == '__main__':
    print(f(10))
    print(f(20))
    print(f(30))
    print(f(1000))
