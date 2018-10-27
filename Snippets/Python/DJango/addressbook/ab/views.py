from django.template import RequestContext
from django.shortcuts import render_to_response
from addressbook.ab.models import User, Contact

def index(request):
	# posts = Post.objects.all()
	# return render_to_response('index.html', {'blog_posts': posts},
	#	context_instance = RequestContext(request))
	return render_to_response('index.html', {},
		context_instance = RequestContext(request))

def login(request):
	username = request.POST['username']
	password = request.POST['password']

	for u in User.objects.all():
		if u.name == username && u.pw == password:
	# p = Post(message = request.POST['message'])
	# p.save()
	# return index(request)
