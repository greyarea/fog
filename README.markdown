h1. Facebook Open Graph client. 

Check out http://developers.facebook.com/docs/reference/api/ for graph request api. 

h2. Example

fog:request(AccessToken, "me/friends", [{fields, "name,picture"}], get).

