// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Generate a simple image gallery from image items using names and notes.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.WorldItems.ImageItem
import bdzimmer.secondary.export.view.{Html, Bootstrap, Markdown}


object ImageGallery {
  
  def render(images: List[ImageItem], size: Int, showCaptions: Boolean): String = {
    Bootstrap.row(
      s"""
<div class="modal fade" id="imagemodal" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-dialog" data-dismiss="modal" style="height:90%;width:70%">
    <div class="modal-content" style="height:100%">              
      <div class="modal-body" style="height:80%;background-color:${Html.ImageBgColorDefault}">
        <button type="button" class="close" data-dismiss="modal"><span aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>
        <img src="" class="imagepreview img-responsive center-block" style="max-height:98%">
      </div> 
      <div class="modal-footer" style="height:20%">
        <div class="col-xs-12">
          <p class="text-left imagedescription"></p>
        </div>
      </div>         
    </div>
  </div>
</div>""" +
"""<style>
body.modal-open {
    position: relative;
}
</style>  
""" +
"""<script>
$(function() {
    $('.pop').on('click', function() {
        $('.imagepreview').attr('src', $(this).find('img').attr('src'));
        var description = $(this).find('img').attr('alt');
        if (description.length > 0) {
            $('.imagedescription').text(description);
            $('.modal-body').css("height", "80%")
            $('.modal-footer').css("height", "20%")
            $('.modal-footer').css("padding", "15px")
        } else {
            $('.modal-body').css("height", "100%")
            $('.modal-footer').css("height", "0%")
            $('.modal-footer').css("padding", "0px")
        }
        $('#imagemodal').modal('show');   
    });     
});
</script>""" + 
      images.map(image => {
        val imageSrc = RenderImages.imagePath(image)
        val caption = if (showCaptions) {
            s"""<div class="caption">${Markdown.processLine(image.name)}</div>"""
        } else {
            ""
        }
        Bootstrap.column(
          size,
          ("""<div class="thumbnail">""" +
          s"""<a class="pop">""" +
          s"""<img src="${imageSrc}" alt="${image.notes}" style="background-color:${Html.ImageBgColorDefault};width:100%"/>""" +
          caption +
          """</a>""" +
          """</div>""")
        )
      }).mkString
    )  
  }
}
