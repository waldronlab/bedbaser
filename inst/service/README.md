BEDbase uses OpenAPI 3.1; however, bedbaser uses the `AnVIL` `Service` class
that imports `rapiclient`, which is restricted to Swagger 2.0. openapi.yaml was
created by converting OpenAPI 3.0 with
[OpenAPI Down Convert](https://github.com/apiture/openapi-down-convert) then
converting the result to Swagger 2.0 with
[api-spec-converter](https://github.com/LucyBot-Inc/api-spec-converter) on a
machine running Ubuntu 24.04.

To recreate openapi.yaml, install `NodeJS` and `npm`.

    sudo apt install nodejs npm
    
Install OpenAPI Down Convert and api-spec-converter with `npm`:

    sudo npm install -g api-spec-converter
    sudo npm install -g @apiture/openapi-down-convert

Download openapi.json from https://api.bedbase.org:

    curl -L -O https://api.bedbase.org/openapi.json
    
Convert openapi.json from OpenAPI 3.1 to 3.0:

    openapi-down-convert --input openapi.json --output openapi_3_0.json
    
Convert from OpenAPI 3.0 to Swagger 2.0 and render in YAML:

    api-spec-converter -f openapi_3 -t swagger_2 openapi_3_0.json \
        --syntax yaml > openapi.yaml
