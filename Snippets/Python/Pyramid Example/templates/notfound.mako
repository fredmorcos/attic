<%inherit file="layout.mako"/>

<div id="notfound">
  <h1>404 - PAGE NOT FOUND</h1>
  The page you're looking for isn't here.
  <a href="${request.route_url('list')}">Go back</a>
</div>
