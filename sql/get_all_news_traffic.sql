BEGIN;
set search_path TO 'central_insights_sandbox';

/*DROP TABLE IF EXISTS vb_news_regions_historic_users_all_news;
CREATE TABLE vb_news_regions_historic_users_all_news
(
    week_commencing     date,
    visitors_raw        bigint,
    requests_raw        bigint,
    visitors            bigint,
    visits_raw          bigint,
    request_per_visitor double precision
) DISTSTYLE AUTO;*/

GRANT ALL on vb_news_regions_historic_users_all_news to vicky_banks with grant option;
GRANT SELECT on vb_news_regions_historic_users_all_news to GROUP central_insights;

--- backfill table
INSERT INTO vb_news_regions_historic_users_all_news
SELECT '<params.run_date>'::date                                                              as week_commencing,
       count(distinct unique_visitor_cookie_id)                                      as visitors_raw,
       count(*)                                                                      as requests_raw,
        round(count(distinct unique_visitor_cookie_id), -4)                           as visitors,
       count(distinct visit_id)                                                      as visits_raw,
       round(requests_raw::double precision / visitors_raw, 1)                       as request_per_browser
FROM s3_audience.audience_activity
WHERE dt >= '<params.run_date>' AND dt < replace(cast('<params.run_date>'::varchar as date) + 7, '-', '')
  AND destination = 'PS_NEWS'
  AND source = 'Events'      -- set to the event scope
  AND content_action IS NULL -- remove any AV things
  AND page_producer NOT ILIKE '%sport%'
  AND page_name != 'keepalive'
  AND app_type != 'web'      -- to exclude the unusual data from interactive maps
GROUP BY 1
ORDER BY visitors_raw desc

;

END;

/*SELECT  week_commencing, visitors FROM vb_news_regions_historic_users_all_news ORDER BY week_commencing;
SELECT  week_commencing,visitors, round(visits_raw,-4) as visits FROM vb_news_regions_historic_users_all_news ORDER BY page_producer, week_commencing;



SELECT DISTINCT week_commencing  FROM vb_news_regions_historic_users_all_news ORDER BY 1 DESC;*/


SELECT DISTINCT week_commencing  FROM vb_news_regions_historic_users_all_news ORDER BY 1 DESC;






