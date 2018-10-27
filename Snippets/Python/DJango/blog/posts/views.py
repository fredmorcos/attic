from django.template import RequestContext
from blog.posts.models import Post
from django.http import HttpResponse
from django.shortcuts import render_to_response, get_object_or_404
from blog.posts.models import Post

def index(request):
	posts = Post.objects.all()
	return render_to_response('index.html', {'blog_posts': posts},
		context_instance = RequestContext(request))

def postMessage(request):
	if request.POST['message'] == "":
		return index(request)

	p = Post(message = request.POST['message'])
	p.save()
	return index(request)

def delete(request, post_id):
	posts = Post.objects.all()
	for i, p in enumerate(posts):
		if int(post_id) == p.id:
			posts[i].delete()
			break
	return index(request)

