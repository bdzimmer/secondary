// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Draw family trees with d3.
// Based on code from this stackoverflow question:
// http://stackoverflow.com/questions/31245751/how-do-you-create-a-family-tree-in-d3-js

// 2015-10-03: Created.
// 2015-10-06: Style fixes. Better mouseovers and links to character pages.
// 2015-10-07: Further updates for preview text. Needs initial zoom / scale based on layout.


var boxWidth = 100, boxHeight = 50;
var bendLocation = 0.4;

var connectX = function (d) {
    return d.x - boxWidth / 2;
};

var connectY = function (d) {
    return d.y - boxHeight / 2;
};

var textX = function (d) {
    return d.x - boxWidth / 2 + 5;
};

var textY = function (d) {
    return d.y - 10;
};

var preview = d3.select("body").append("div")
  .attr("class", "preview")
  .style("position", "absolute")
  .style("opacity", 0);

function drawTree(root, id, width, height)  {

	
  // TODO: calculate layout first and set initial zoom
	
  // zoomable SVG
  var svg = d3.select(id)
    .append("svg")
    .attr("width", width + "px")
    .attr("height", height + "px")
    .call(d3.behavior.zoom().on("zoom", function () {
      svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
    }))
    .append("g")
    
  var flatNodes = flatten(root);

  // compute layout
  var tree = d3.layout.tree().nodeSize([boxWidth * 1.1, boxHeight * 2])  //.size([width - boxWidth, height - boxHeight]);
  var links = tree.links(tree.nodes(root));

  // add link lines
  svg.selectAll(".link")
    .data(links)
    .enter().append("path")
    .attr("class", function(d) {
      return d.target.parent_type;
    })
    .attr("d", elbow);

  var nodes = svg.selectAll(".node")
    .data(tree.nodes(root))
    .enter();

  // add spouse lines
  svg.selectAll(".spouse")
    .data(spouses)
    .enter().append("path")
    .attr("class", "spouse")
    .attr("d", function(d) {
    	spouseLine(d, flatNodes);
    });

  
  // probably a better way to do this with groups
  
  // add node rectangles
  var groups = nodes.append("a")
  	.attr("xlink:href", function(d){return d.id + ".html";});
 
  	
  groups.append("rect")
    .attr("class", "node")
    .attr("height", boxHeight)
    .attr("width", boxWidth)
    .attr("id", function (d){return d.id;})
    .attr("display", function (d) {
      if (d.hidden) {
        return "none";
      } else {
        return "";
      };
    })
    .attr("x", connectX)
    .attr("y", connectY)
  	.on("mouseenter", mousenter)
    .on("mouseleave", mouseleave)
    .on("mousemove", mousemove);
    
  
  groups.append("text")
    .attr("class", "nodetext")
    .text(function (d){return d.name;})
    .attr("x", textX)
    .attr("y", textY)
    .on("mouseenter", mousenter)
    .on("mouseleave", mouseleave)
    .on("mousemove", mousemove);
    
}

// draw a spouse line
function spouseLine(d, flatNodes) {

  //start point
  var startNode = flatNodes.filter(function (v) {
    return (d.srcId === v.id);
  });
  //end point
  var endNode = flatNodes.filter(function (v) {
    return (d.dstId === v.id);
  });

  // line start and end
  var lineData = [{x: startNode[0].x, y: startNode[0].y},
                  {x: endNode[0].x, y: endNode[0].y}];

  var fun = d3.svg.line()
    .x(function (d) { return d.x; })
    .y(function (d) { return d.y; })
    .interpolate("linear");

  return fun(lineData);
}


// flatten the node list
// I think I can get rid of the stuff to add node ids.
function flatten(root) {
  var n = [];
  
  // var i = 0;

  function recurse(node) {
    if (node.children) {
    	node.children.forEach(recurse);
    }
    // if (!node.id) {
    // 	node.id = ++i;
    // }
    n.push(node);
  }
  recurse(root);

  return n;
}


// draw right angle lines
function elbow(d, i) {

  if (d.target.parent_type === "none") {
      return "M0,0L0,0";
  }

  var diff = d.source.y - d.target.y;
  var ny = d.target.y + diff * bendLocation;

  var lineData = [{x: d.target.x, y: d.target.y},
                  {x: d.target.x, y: ny},
                  {x: d.source.x, y: d.source.y}]

  var fun = d3.svg.line()
  .x(function (d) { return d.x; })
  .y(function (d) { return d.y; })
  .interpolate("step-after");
  
  return fun(lineData);
}

// show / hide preview when mousing
function mousenter(d) {
  preview.html("<h4>" + d.name + "</h4>" + d.description)
    .style("opacity", 1);
}

function mouseleave(d) {
  preview.html("").style("opacity", 0);
}

function mousemove(d) {
  preview
    .style("left", d3.event.pageX + 10 + "px")
    .style("top",  d3.event.pageY + 10 + "px");
}



// Currently not using
/*
d3.selection.prototype.moveToFront = function() {
  return this.each(function(){
    this.parentNode.appendChild(this);
  });
};

d3.selection.prototype.moveToBack = function() { 
    return this.each(function() { 
        var firstChild = this.parentNode.firstChild; 
        if (firstChild) { 
            this.parentNode.insertBefore(this, firstChild); 
        } 
    }); 
};
*/	