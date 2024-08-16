FROM rapporteket/base-r:4.2.2

LABEL maintainer "Arnfinn Hykkerud Steindal <arnfinn.hykkerud.steindal@helse-nord.no>"

LABEL no.rapporteket.cd.enable="true"

WORKDIR /app/R

# hadolint ignore=DL3010
COPY *.tar.gz .

RUN R -e "remotes::install_local(list.files(pattern = \"*.tar.gz\"))" \
    && rm ./*.tar.gz

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port = 3838,shiny.host = \"0.0.0.0\"); rapRegTemplate::run_app()"]
