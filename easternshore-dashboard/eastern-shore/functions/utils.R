accordianComponent <- function(id, btnText, content, id2, id3){
  HTML(paste0('
  <div class="accordion accordion-flush" id="',id2,'">
  <div class="accordion-item">
    <h2 class="accordion-header" id="',id3,'">
      <button class="accordion-button collapsed" type="button" data-bs-toggle="collapse" data-bs-target="#',id,'" aria-expanded="false" aria-controls="',id,'">
        ',btnText,'
      </button>
    </h2>
    <div id="',id,'" class="accordion-collapse collapse" aria-labelledby="',id3,'" data-bs-parent="#',id2,'">
      <div class="accordion-body">', content, '</div>
    </div>
  </div>
  </div>
  '))
}

cardComponent <- function(content){
  HTML(paste0('
  <div class="card">
    <div class="card-body" style="padding-top:0;">
    ', content, '
    </div>
  </div>
  '))
}

cardComponentSelect <- function(select, accordian){
  HTML(paste0('
  <div class="card">
    <div class="card-body">
    ', select, accordian, '
    </div>
  </div>
  '))
}

cardComponentSelectGeo <- function(check, radio, action){
  HTML(paste0('
  <div class="card">
    <div class="card-body">
    ', check, radio, action, '
    </div>
  </div>
  '))
}


collapsibleButton <- function(id, btnText, content){
  HTML(paste0('
  <button class="btn btn-primary" type="button" data-toggle="collapse" data-target="#',id,' " aria-expanded="false" aria-controls="collapseExample">
    ',btnText,'
  </button>
  <div class="collapse in" id="',id,'">
    <div class="var-exp">
    ', content,'
    </div>
  </div>
  '))
}
