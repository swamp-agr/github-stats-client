Github API from scratch
================

Goal is to find users with particular criteria, e.g. language or location

Understanding Github API
========================

Two endpoints for start: /users and /search for users.
For sure, for this particular task it is better to use search because of search criterias.

Users could be obtained w/ or w/o Token.

Server defines API calls speed. w/ authentication speed is higher rather than w/o.

Search for users also had limit. 100 per request, 1000 per search. Search could be tuned with several parameters. The most preferrable is `created`. Range could be passed. Created - to fit search limits.

Otherwise, you will see following message:
```
{
  "message": "Only the first 1000 search results are available",
  "documentation_url": "https://developer.github.com/v3/search/"
}
```


There are two issues:

* to obtain list of ranges with minimal server requests.
* to iterate through the list of ranges to obtain the list of users.

Request Headers
---------------

* OAuthV2 Token could be passed in Header:

```
Authorization: token <YOUR_TOKEN_HERE>
```

* Acceptance of API, constant string:

```
Accept: application/vnd.github.v3.text-match+json
```

* Customer User-Agent. It should contain username and/or repository for contact in case of any issues:

```
User-Agent: <YOUR_USER_AGENT>
```


Response Headers
----------------

X-RateLimit-Limit: 30
X-RateLimit-Remaining: 29
X-RateLimit-Reset: 1506545852

Out of limit issue had not been tested since it will affect RateLimit.

Calls
-----

- obtain total for period: https://api.github.com/search/users?q=location:%3ELOCATION+type:user+created:RANGE&sort=created&per_page=1
- https://api.github.com/search/users?q=location:%3ELOCATION+type:user+created:RANGE&sort=created&per_page=100&page=N
- Range format for RANGE: 2006-01-01..2007-01-01 or even time included
- page values for N: from 1 to 10

So, following modules required:
* Config for Token, User-Agent, location
* CLI for debug/run modes.
* Types
* API:
  - for manipulating with headers. Setting request headers, getting response headers.
  - for tuning of request's speed.
  - for making calls and getting responses.
* Business Logic:
  - for applying algorythm of obtaining ranges.
  - for retrieving users list.