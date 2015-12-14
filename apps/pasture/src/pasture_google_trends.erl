-module(pasture_google_trends).
-compile(export_all).

test() ->
    Url = "https://go.pardot.com/l/23452/2015-08-28/2znfhw",
    Headers = [
                {"Host", "go.pardot.com"},
                {"Accept", "*/*"},
                {"Accept-Language", "en-US,en;q=0.5"},
                {"Accept-Encoding", "gzip, deflate"},
                {"Content-Type", "application/x-www-form-urlencoded; charset=UTF-8"},
                {"Referer", "https://www.erlang-solutions.com/"},
                % {"Content-Length", "144"},
                {"Origin", "https://www.erlang-solutions.com"},
                {"Connection", "keep-alive"},
                {"Pragma", "no-cache"},
                {"Cache-Control", "no-cache"}
    ],
    Body = "name=name2&surname=surname2&email=email2%40email.com"
           "&company=company2&job_title=job-title2&country=country2"
           "&pardot_extra_field=&message=message2",
    ibrowse:send_req(Url, Headers, post, Body).

    % URL
    % https://go.pardot.com/l/23452/2015-08-28/2znfhw

    % HEADERS
    % Host: go.pardot.com
    % User-Agent: Mozilla/5.0 (Windows NT 6.1; rv:38.0) Gecko/20100101 Firefox/38.0
    % Accept: */*
    % Accept-Language: en-US,en;q=0.5
    % Accept-Encoding: gzip, deflate
    % Content-Type: application/x-www-form-urlencoded; charset=UTF-8
    % Referer: https://www.erlang-solutions.com/
    % Content-Length: 137
    % Origin: https://www.erlang-solutions.com
    % Connection: keep-alive
    % Pragma: no-cache
    % Cache-Control: no-cache

    % BODY: 
    % name=name&surname=surname&email=email%40email.com&company=company&job_title=job-title&country=country&pardot_extra_field=&message=message