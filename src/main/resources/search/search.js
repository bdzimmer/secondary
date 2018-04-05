function search(id, items) {
  // insert a search bar with responsive filtering functionality
  var itemsObject = {};
  for (var i = 0; i < items.length; i++) {
    itemsObject[items[i].id] = items[i];
  }
  document.write("<span class=\"search-custom\">");
  document.write("<input id=\"" + id + "-text\" type=\"text\" placeholder=\"Search\" />");
  document.write("<div id=\"" + id + "-search-custom-content\" class=\"search-custom-content\">");
  for (var i = 0; i < items.length; i++) {
     var item = items[i];
     document.write("<a href=\"" + item.link + "\" id=\"" + item.id + "\">" + item.name + "</a>");
  }
  document.write("</div>");
  document.write("</span>");
  var text = document.getElementById(id + "-text");
  text.onkeyup = function(event){filterSearch(event, id, itemsObject)};
}

function filterSearch(event, id, itemsObject) {
  // update visibility of search items
  var search = document.getElementById(id + "-search-custom-content");
  var text = document.getElementById(id + "-text").value.toLowerCase();

  // if no text is entered, don't show any results
  if (text.length == 0) {
    search.style.display = "none";
    return;
  } 
  
  // find links that match search text
  var aList = search.getElementsByTagName("a");
  var visLinks = [];
  for (var aIdx = 0; aIdx < aList.length; aIdx++) {
    var a = aList[aIdx];
    var item = itemsObject[a.id];
    var match = false;
    for (var tagIdx = 0; tagIdx < item.tags.length; tagIdx++) {
      var tag = item.tags[tagIdx];
      if (tag.startsWith(text)) {
        // console.log("match " + text + " " + tag)
        match = true;
        break;
      }
    }
    // if text matched a tag, make the link visible
    // and add to list of visible links
    if (match) {
      a.style.display = "block";
      visLinks.push(item.link);
    } else {
      a.style.display = "none";
    }
  }
  
  // only display result div if there are results
  if (visLinks.length > 0) {
    search.style.display = "block";
  } else {
    search.style.display = "none";
  }
  
  // if enter was pressed, navigate to the first result
  if (event.keyCode == 13 && visLinks.length > 0) {
    window.location.href = visLinks[0];
  }
  
}
