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

Algorythm
=========

0. get total count of users for whole period.
1. get total count of users for each year.
2. get total count of users for each month for those ranges where were too many users.
3. get total count of users for each week for those ranges where were too many users.
4. get total count of users for each day for those ranges where were too many users.
5. for each period iterate through the pages of each query

too many = (> 1000)

Interaction
-----------

1. API count call

1 call: Headers: token, user-agent, constant
Params: per_page=1, page=1, q= calculated
header in response: 
X-RateLimit-Limit: 30
X-RateLimit-Remaining: X
X-RateLimit-Reset: Y

many calls: before each new call calculate thread delay: 
if X-RateLimit is equal to 30 then 60 sec / 30 = 2 sec.
if X-RateLimit-Remaining: >0 then iterate over rest requests. Otherwise, calculate delay based on X-RateLimit-Reset and wait till limit reset.

Prerequisites
=============

Due to https://github.com/snoyberg/http-client/issues/302 timeout cannot be properly handled with wreq and all other clients based on `http-client`. In this particular case we will try to change client from `wreq` to `curl` itself.

1. Obtain Authorization Token OAuth2 for Github with `Users` permission.
2. Fill `token` value in `sample.yml` file with obtained token.
3. Change `location` to your value.
4. `stack` utility installed.

Installation
============

```
stack build
```

