from django.db import models

class Post(models.Model):
	message = models.CharField(max_length = 200)

