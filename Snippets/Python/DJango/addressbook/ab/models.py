from django.db import models

class User(models.Model):
	name = models.CharField(max_length = 200)
	pw = models.CharField(max_length = 10)

class Contact(models.Model):
	user = models.ForeignKey(User)
	name = models.CharField(max_length = 200)
	address = models.CharField(max_length = 200)

