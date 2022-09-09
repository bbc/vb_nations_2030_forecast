BEGIN;
set search_path TO 'central_insights_sandbox';

DROP TABLE IF EXISTS vb_news_regions_num_pages;
CREATE TABLE vb_news_regions_num_pages
(
    week_commencing date,
    page_producer   varchar(400),
    pages           bigint
) DISTSTYLE AUTO;

GRANT ALL on vb_news_regions_num_pages to vicky_banks with grant option;
GRANT SELECT on vb_news_regions_num_pages to GROUP central_insights;

--- backfill table
INSERT INTO vb_news_regions_num_pages
with get_pages as (
SELECT DISTINCT '<params.run_date>'::date                                                              as week_commencing,
                CASE
                    WHEN page_producer ILIKE '%English%' THEN 'England'
                    WHEN page_producer ILIKE '%Wales%' THEN 'Wales'
                    WHEN page_producer ILIKE '%Scotland%' THEN 'Scotland'
                    WHEN page_producer ILIKE '%Northern Ireland%' THEN 'Northern Ireland' END as page_producer,
                REGEXP_REPLACE(REGEXP_REPLACE(REGEXP_REPLACE(page_name, '-', ' '), 'other::homepage::sport.app.page',
                                     'other::homepage::sport.page'), 'sport.page',
                      'other::homepage::sport.page') AS clean_page_name, count(distinct visit_id) as visits
FROM s3_audience.audience_activity
WHERE dt >= '<params.run_date>' AND dt < replace(cast('<params.run_date>'::varchar as date) + 7, '-', '')
  AND destination = 'PS_NEWS'
  AND source = 'Events' -- set to the event scope
  AND content_action IS NULL -- remove any AV things
  AND page_producer NOT ILIKE '%sport%'
  AND page_name != 'keepalive'
  AND app_type != 'web' -- to exclude the unusual data from interactive maps
  AND (page_producer ILIKE '%Wales%' OR
       page_producer ILIKE '%Scotland%' OR
       page_producer ILIKE '%Ireland%' OR
       page_producer ILIKE '%English%')
GROUP BY 1,2,3
HAVING visits >=100
ORDER BY visits asc
)

SELECT week_commencing, page_producer, count(distinct clean_page_name) as pages
FROM get_pages
GROUP BY 1,2
ORDER BY 1,2;

;

END;

/*SELECT  week_commencing, page_producer, visitors FROM vb_news_regions_num_pages ORDER BY page_producer, week_commencing;
SELECT  week_commencing, page_producer, visitors, round(visits_raw,-4) as visits FROM vb_news_regions_num_pages ORDER BY page_producer, week_commencing;



SELECT DISTINCT week_commencing  FROM vb_news_regions_num_pages ORDER BY 1 DESC;*/