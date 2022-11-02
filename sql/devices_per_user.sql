-- find how many cookies per person over time

with get_cookies_per_person as (
    SELECT audience_id, count(distinct unique_visitor_cookie_id)::double precision as num_cookies
    FROM s3_audience.audience_activity
    WHERE dt BETWEEN '20220501' AND '20220701'
      AND (destination = 'PS_NEWS' or destination = 'GNL_NEWS' or
           destination = 'OTHER_SOCIAL_SYNDICATION')
      AND (geo_country_site_visited = 'United Kingdom' or geo_country_site_visited = 'Isle of Man'
        or geo_country_site_visited = 'Jersey' or geo_country_site_visited = 'Guernsey')
    AND is_signed_in = TRUE AND is_personalisation_on = TRUE
    GROUP BY 1
)
SELECT median(num_cookies), avg(num_cookies), stddev(num_cookies)
FROM get_cookies_per_person;
/*

median,avg,stddev
1,1.8684122087338495,2.1737169423350315

avg = 1.9 cookies per person


 */

--- old dates
with get_cookies_per_person as (
    SELECT audience_id, count(distinct unique_visitor_cookie_id)::double precision as num_cookies
    FROM s3_audience.audience_activity
    WHERE dt BETWEEN '20190501' AND '20190701'
      AND (destination = 'PS_NEWS' or destination = 'GNL_NEWS' or
           destination = 'OTHER_SOCIAL_SYNDICATION')
      AND (geo_country_site_visited = 'United Kingdom' or geo_country_site_visited = 'Isle of Man'
        or geo_country_site_visited = 'Jersey' or geo_country_site_visited = 'Guernsey')
    AND is_signed_in = TRUE AND is_personalisation_on = TRUE
    GROUP BY 1
)
SELECT median(num_cookies), avg(num_cookies), stddev(num_cookies)
FROM get_cookies_per_person;

/*
 median,avg,stddev
1,1.7744029133914208,2.7048692057457924


 average = 1.8 cookies per person so basically same
 */


with get_cookies_per_person as (
    SELECT audience_id, count(distinct unique_visitor_cookie_id) as num_cookies
    FROM s3_audience.audience_activity
    WHERE dt BETWEEN '20220201' AND '20220401'
      AND (destination = 'PS_NEWS' or destination = 'GNL_NEWS' or
           destination = 'OTHER_SOCIAL_SYNDICATION')
      AND (geo_country_site_visited = 'United Kingdom' or geo_country_site_visited = 'Isle of Man'
        or geo_country_site_visited = 'Jersey' or geo_country_site_visited = 'Guernsey')
    GROUP BY 1
)
SELECT num_cookies, count(distinct audience_id) as users
FROM get_cookies_per_person
GROUP BY 1
ORDER BY 1;


