<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Markr</title>

    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <meta name="description" content="Share Your Bookmarks!"/>

    <link rel="shortcut icon" href="${request.static_url('markr:static/favicon.ico')}"/>
    <link rel="stylesheet"
          href="${request.static_url('markr:static/css/bootstrap.min.css')}"/>
    <link rel="stylesheet"
          href="${request.static_url('markr:static/css/bootstrap-responsive.min.css')}"/>
    <style>
      body {
        padding-top: 60px;
      }
    </style>

    <!-- <script src="${request.static_url('markr:static/js/bootstrap.min.js')}"/> -->
  </head>
  <body>
    <div class="navbar navbar-inverse navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </a>
          <a class="brand" href="/">Markr</a>
          <div class="nav-collapse collapse">
            <ul class="nav">
              <li class="active"><a href="/">Home</a></li>
              <li><a href="/register">Register</a></li>
              <li><a href="/login">Login</a></li>
              <li><a href="/about">About</a></li>
              <li><a href="/contact">Contact</a></li>
            </ul>
          </div>
        </div>
      </div>
    </div>
    <div class="container">
      ${next.body()}
    </div>
  </body>
</html>
