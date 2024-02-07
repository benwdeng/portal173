
FROM rocker/shiny

# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
	
ENV _R_SHLIB_STRIP_=true

RUN install2.r --error --skipinstalled \
    data.table \
	shiny \
	shinydashboard \
	DT \
	shinyalert \
	shinyBS \
	shinyjs \
	officer \
	rmarkdown \
	leaflet \
	futile.logger\
	
	crosstalk\
	htmlwidgets \
	janitor \
	kableExtra \
	lazyeval \
	plotly \
	snakecase \
	svglite 

RUN addgroup --system app \
    && adduser --system --ingroup app app

COPY ./app /home/app

RUN chown app:app -R /home/app

USER app

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app', host = '0.0.0.0', port = 3838)"]