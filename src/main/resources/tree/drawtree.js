// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Draw family trees with d3.
// Based on code from this stackoverflow question:
// http://stackoverflow.com/questions/31245751/how-do-you-create-a-family-tree-in-d3-js

// 2015-10-03: Created.
// 2015-10-06: Style fixes. Better mouseovers and links to character pages.
// 2015-10-07: Further updates for preview text. Needs initial zoom / scale based on layout.
// 2015-10-08: Initial zoom / scale.

// TODO: consistent variable names; spouse lines and marriage nodes

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

  // define layout; compute node and link positions
  var layout = d3.layout.tree().nodeSize([boxWidth * 1.1, boxHeight * 2]);
  var nodes = layout.nodes(root);
  var links = layout.links(nodes);
  
  // figure out initial translation and scale
  // it appears that the translation and scale have to be applied to both
  // the main svg group and the zoom listener for consistent results.
  
  var min_x = d3.min(nodes, function(d) {return d.x;}) - boxWidth / 2;
  var max_x = d3.max(nodes, function(d) {return d.x;}) + boxWidth / 2;
  var min_y = d3.min(nodes, function(d) {return d.y;}) - boxHeight / 2;
  var max_y = d3.max(nodes, function(d) {return d.y;}) + boxHeight / 2;
  
  // TODO: consider height in start_scale
  var start_scale = (max_x - min_x) / width
  var trans_x = 0 - min_x / start_scale
  var trans_y = 0 - min_y / start_scale
  
  console.log(trans_x + " " + trans_y + " " + start_scale);
  
  // create zoom object for initial layout that allows scaling and translation
  var zoom = d3.behavior.zoom()
    .on("zoom", function () {
      svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
  	})
  	.translate([trans_x, trans_y])
  	.scale(start_scale);

  // create SVG using zoom object
  var svg = d3.select(id)
    .append("svg")
    .attr("width", width + "px")
    .attr("height", height + "px")
    .call(zoom)
    .append("g")
    .attr("transform", "translate(" + [trans_x, trans_y] + ") scale(" + start_scale + ")");
    
  
  var flatNodes = flatten(root);

  // add link lines
  svg.selectAll(".link")
    .data(links)
    .enter().append("path")
    .attr("class", function(d) {
      return d.target.parent_type;
    })
    .attr("d", elbow);


  // add spouse lines
  svg.selectAll(".spouse")
    .data(spouses)
    .enter().append("path")
    .attr("class", "spouse")
    .attr("d", function(d) {
    	spouseLine(d, flatNodes);
    });

  
  // add node rectangles
  var groups = svg.selectAll(".node")
	.data(nodes)
	.enter().append("a")
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
