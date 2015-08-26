// Put code in an Immediately Invoked Function Expression (IIFE).
// This isn't strictly necessary, but it's good JavaScript hygiene.
(function() {

// See http://rstudio.github.io/shiny/tutorial/#building-outputs for
// more information on creating output bindings.
// Generic output binding for NVD3 multi-bar chart
var d3_corr_out_bind = new Shiny.OutputBinding();

d3_corr_out_bind.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".d3-correlations");
};

d3_corr_out_bind.renderValue = function(el, data) {
  var cells, colorScale, colors, corXscale, corYscale, corZscale, corr, corrplot, drawScatter, h, i, innerPad, j, nGroup, nind, nvar, pad, scatterplot, svg, totalh, totalw, w;
  h = 300
  w = h
  pad = {
    left:200, 
    top:20, 
    right:10, 
    bottom: 20
  };
  innerPad = 5     

  totalh = h + pad.top + pad.bottom
  totalw = (w + pad.left + pad.right)*2

  //remove the old graph
  var svg = d3.select(el).select("svg");
  svg.remove();
  $(el).html("");

  //append a new one
  svg = d3.select(el)
          .append("svg")
          .attr("height", totalh)
          .attr("width", totalw)

  // panel for correlation image
  corrplot = svg.append("g").attr("id", "corrplot").attr("transform", "translate(" + pad.left + "," + pad.top + ")");
  
  // panel for scatterplot
  // scatterplot = svg.append("g").attr("id", "scatterplot").attr("transform", "translate(" + (pad.left * 2 + pad.right + w) + "," + pad.top + ")");

  // no. data points
  nind = data.ind.length;
  nvar = data["var"].length;

  corXscale = d3.scale.ordinal().domain(d3.range(nvar)).rangeBands([0, w]);
  corYscale = d3.scale.ordinal().domain(d3.range(nvar)).rangeBands([h, 0]);
  corZscale = d3.scale.linear().domain([-1, 0, 1]).range(["crimson", "white", "darkslateblue"]);

  // create list with correlations
  corr = [];
  for (i in data.cor) {
    for (j in data.cor[i]) {
      corr.push({
        row: i,
        col: j,
        value: data.cor[i][j]
      });
    }
  }
  
  cells = corrplot.selectAll("empty").data(corr).enter().append("rect").attr("class", "cell").attr("x", function(d) {
    return corXscale(d.col);
  }).attr("y", function(d) {
    return corYscale(d.row);
  }).attr("width", corXscale.rangeBand()).attr("height", corYscale.rangeBand()).attr("fill", function(d) {
    return corZscale(d.value);
  }).attr("stroke", "none").attr("stroke-width", 2).on("mouseover", function(d) {
    d3.select(this).attr("stroke", "black");
    corrplot.append("text").attr("id", "corrtext").text(d3.format(".2f")(d.value)).attr("x", function() {
      var mult;
      mult = -1;
      if (d.col < nvar / 2) {
        mult = +1;
      }
      return corXscale(d.col) + mult * corXscale.rangeBand() * 5;
    }).attr("y", function() {
      var mult;
      mult = +1;
      if (d.row < nvar / 2) {
        mult = -1;
      }
      return corYscale(d.row) + (mult + 0.5) * corYscale.rangeBand() * 2;
    }).attr("fill", "black").attr("dominant-baseline", "middle").attr("text-anchor", "middle");
    corrplot.append("text").attr("class", "corrlabel").attr("x", corXscale(d.col)).attr("y", h + pad.bottom * 0.4).text(data["var"][d.col]).attr("dominant-baseline", "middle").attr("text-anchor", "middle");
    return corrplot.append("text").attr("class", "corrlabel").attr("y", corYscale(d.row)).attr("x", -pad.left * 0.02).text(data["var"][d.row]).attr("dominant-baseline", "middle").attr("text-anchor", "end");
  }).on("mouseout", function() {
    d3.selectAll("text.corrlabel").remove();
    d3.selectAll("text#corrtext").remove();
    return d3.select(this).attr("stroke", "none");
  }).on("click", function(d) {
    debugger
    var result = {
      col : d.col, 
      row : d.row,
      cor : Math.abs(d.value)
    }
    Shiny.onInputChange("pair-wise-collinear",result)
  });

  // boxes around panels
  corrplot.append("rect")
         .attr("height", h)
         .attr("width", w)
         .attr("fill", "none")
         .attr("stroke", "black")
         .attr("stroke-width", 1)
         .attr("pointer-events", "none")

  // text above
  corrplot.append("text")
          .text("Correlation matrix")
          .attr("id", "corrtitle")
          .attr("x", w/2)
          .attr("y", -pad.top/2)
          .attr("dominant-baseline", "middle")
          .attr("text-anchor", "middle")
};

// Tell Shiny about the output binding
Shiny.outputBindings.register(d3_corr_out_bind, "d3-correlations-out");

var d3_bubbles_out_bind = new Shiny.OutputBinding();

d3_bubbles_out_bind.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".d3-bubbles");
};

d3_bubbles_out_bind.renderValue = function(el, thedata) {

  var featureID = thedata.ylabel;
  var data = [];
  var minmax = [thedata.range.min,thedata.range.max];
  // var totalw = $(el).width();
  // var totalh = $(el).height();
  var totalw = 850;
  var totalh = 450;
  var margin = {
    top:30,
    bottom:30,
    left:30,
    right:30
  };

  var colors = d3.scale.category20();

  //remove the old graph
  var svg = d3.select(el).select("svg");
  svg.remove();
  $(el).html("");

  //append a new one
  svg = d3.select(el)
          .append("svg")
          .attr("height", totalh) //- margin.top - margin.bottom
          .attr("width", totalw) //- margin.top - margin.bottom

  function convertRange( value, r1, r2 ) { 
      return ( value - r1[ 0 ] ) * ( r2[ 1 ] - r2[ 0 ] ) / ( r1[ 1 ] - r1[ 0 ] ) + r2[ 0 ];
  }
  
  colors("Original category")
  colors("NAs")
  colors("Empty Strings")
  colors("Clubbed category")

  // Like other layouts, d3 expects objects that the layout can manipulate
  thedata.data.forEach(function(item, index) {
    var r = 5;
    var diff = (minmax[1] - minmax[0]);
    if (diff > 0) {
      if (diff <= r)
        r = convertRange(item.y, minmax, [r, (minmax[1] - minmax[0]) + r]) // 
      else if (diff <= 200)
        r = convertRange(item.y, minmax, [r, ((minmax[1] - minmax[0])/5) + r]) // 
      else // if (diff <= 100)
        r = convertRange(item.y, minmax, [r, ((minmax[1] - minmax[0])/10) + r]) // 
    }

    var fixed = false;
    var color = colors("Original category")

    if (item.x === "NA")
      // NA!
      color = colors("NAs")
    else if (item.x === "")
      // empty string!
      color = colors("Empty Strings")

    if (thedata.modified.indexOf(item.x) >= 0)
      // match!
      color = colors("Clubbed category")

    data[index] = {
      label:item.x,
      value: item.y,
      radius: r,
      color:color,
      fixed:fixed
    }
  })

  var force = d3.layout.force()
      .nodes(data)

      // size affects center of gravity
      .size([totalw, totalh])

      // defaults to .9
      // 1 = frictionless, 0 = frozen
      .friction(0.5)

      // defaults to -30
      // can be a function
      // A negative value results in node repulsion, while a positive value results in node attraction.
      // 0 charge disables the quadtree computation
      .charge(function(d) { 
        return -(d.radius * d.radius)/0.8; 
        // return -d.radius;
      })

      // deftaults to 0.8
      // sets approximation
      .theta(1)
      
      // defaults to 0.1
      // 0 = disabled
      .gravity(0.3)

      // gets or sets the cooling param
      .alpha(0.9)
      
      // this is necessary for the animation to be reprensented 
      .on("tick", tick)

      // start ticking internally
      .start();

  var drag_node = d3.behavior.drag()
      .origin(function() {
          var t = d3.transform(d3.select(this).attr("transform")).translate;
          return {x: t[0], y: t[1]};
       })
      .on("dragstart", dragstart)
      .on("drag", dragmove)
      .on("dragend", dragend);

  function dragstart(d, i) {
    $('.popover').each(function() {
      $(this).remove();
    }); 
    force.stop() // stops the force auto positioning before you start dragging
    d3.selectAll('.node').on("mouseover",gobbleBubble).on("mouseout",releaseBubble);
    d3.select(this).on("mouseover",null).on("mouseout",null);
    // d3.select(this).classed("dragging", true);
    $(this).insertBefore($(this.parentElement.children[0]))
  }

  function dragmove(d, i) {
    d3.select(this).transition().ease("elastic").attr("transform", "translate(" + d3.event.x + "," + d3.event.y + ")");
  }

  function dragend(d, i) {
    showTooltip.call(this,d);
    d3.selectAll('.node').on("mouseover",function(d) {
      showTooltip.call(this,d);
    }).on("mouseout", hideTooltip);
    
    if (target !== null) {
      d3.select("circle.highlighter").remove();
      Shiny.onInputChange(thedata.evt, {
        featureID : featureID,
        dragged : d.label,
        target : target.label,
        random : Math.random()
      })  
    }
    target = null;
  }

  var target = null;
  var gobbleBubble = function(d,i) {
    target = d;

    d3.select(this).append("circle").attr("class","highlighter").attr('r', function(d) {
      return d.radius + 5;
    });
  };

  var releaseBubble = function(d,i) {
    target = null;
    d3.select("circle.highlighter").remove();
  };

  var showTooltip = function(d,i) {
    $(this).popover({
            placement: 'auto top',
            container: 'body',
            trigger: 'manual',
            html : true,
            content: function() { 
              return "<b>Category:</b> " + d.label + "<br />" +
              "<b>Counts:</b> " + d.value + "<br />"; 
            }
          });
    $(this).popover('show')
  };

  var hideTooltip = function(d,i) {
    $('.popover').each(function() {
      $(this).remove();
    }); 
  };

  // this returns extended objects
  var node = svg.selectAll('.node')
     .data(force.nodes())
     .enter().append('g')
     .attr('class', "node").call(drag_node);

  node
      .append('circle')
      .attr('r', function(d) {
        return d.radius || 5
      }).style("fill", function(d) {
        return d.color;
      })

  node.on("mouseover", function(d) {
    showTooltip.call(this,d);
  }).on("mouseout", hideTooltip);

  var legend = svg.selectAll(".legend")
      .data(colors.domain())
      .enter().append("g")
      .attr("class", "legend")
      .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });

  legend.append("rect")
      .attr("x", totalw - 18)
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", colors);

  legend.append("text")
      .attr("x", totalw - 24)
      .attr("y", 9)
      .attr("dy", ".35em")
      .style("text-anchor", "end")
      .text(function(d) { return d; });

  function tick() {
    node.attr("transform", function(d) {
      var r = d.radius;
      d.x = Math.max(r, Math.min(totalw - r, d.x));
      d.y = Math.max(r, Math.min(totalh - r, d.y));
      return "translate(" + d.x + "," + d.y + ")"; 
    });
  }
  
};

// Tell Shiny about the output binding
Shiny.outputBindings.register(d3_bubbles_out_bind, "d3-bubbles-out");

var d3_bubbles_clusters_bind = new Shiny.OutputBinding();

d3_bubbles_clusters_bind.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".d3-bubbles-clusters");
};

d3_bubbles_clusters_bind.renderValue = function(el, thedata) {
  debugger
  var featureID = thedata.ylabel;
  var data = [];
  var scale = d3.scale.linear().domain([ thedata.range.min, thedata.range.max ]).range([ 5, 25 ]);
  // var totalw = $(el).width();
  // var totalh = $(el).height();
  var totalw = 1300;
  var totalh = 500;
  var margin = {
    top:30,
    bottom:30,
    left:120,
    right:80
  };
  var legendX = totalw - margin.right;
  var width = totalw - margin.left - margin.right;
  var height = totalh - margin.top - margin.bottom;
  var truncateLen = 10;

  var index = 0;
  var features = [];
  for(group in thedata.data) {
    for(sub in thedata.data[group]) {
      var node = thedata.data[group][sub];
      
      var r = scale(node);
      // var color = colors("Original category")

      data[index] = {
        // label:item,
        value: node,
        radius: r,
      }
      data[index][thedata.feature] = sub;
      data[index][thedata.groupby] = group;
      features.push(sub)
      index++;
    }
  }

  features = _.uniq(features);
  var keys = Object.keys(thedata.data);
  var colors = d3.scale.category20().domain(features);

  //remove the old graph
  var svg = d3.select(el).select("svg");
  svg.remove();
  $(el).html("");

  //append a new one
  svg = d3.select(el)
          .append("svg")
          .attr("height", totalh) //- margin.top - margin.bottom
          .attr("width", totalw) //- margin.top - margin.bottom
  
  // colors("Original category")
  // colors("NAs")
  // colors("Empty Strings")
  // colors("Clubbed category")


  var getCenters = function (size) {
    var centers, map;
    centers = keys.map(function (d) {
      return {name: d, value: 1};
    });
    
    map = d3.layout.treemap().size(size).ratio(1/1);
    map.nodes({children: centers});

    // map = d3.layout.pack().size(size);
    // map.nodes({children: centers}); 

    return centers;
  };

  var force = d3.layout.force()
      .nodes(data)

      // size affects center of gravity
      .size([width, height])

      // defaults to .9
      // 1 = frictionless, 0 = frozen
      .friction(0.9)

      // defaults to -30
      // can be a function
      // A negative value results in node repulsion, while a positive value results in node attraction.
      // 0 charge disables the quadtree computation
      .charge(function(d) { 
        return -(d.radius * d.radius)/0.6; 
        // return -d.radius;
        // return -100;
      })

      // deftaults to 0.8
      // sets approximation
      .theta(1)
      
      // defaults to 0.1
      // 0 = disabled
      .gravity(0.1)

      // gets or sets the cooling param
      .alpha(0.9)

  var showTooltip = function(d,i) {
    $(this).popover({
            placement: 'auto top',
            container: 'body',
            trigger: 'manual',
            html : true,
            content: function() { 
              return "<b>" + thedata.feature + ":</b> " + d[thedata.feature] + "<br />" + 
                        "<b>Counts:</b> " + d.value + "<br />" + 
                        "<b>" + thedata.groupby + "</b> " + d[thedata.groupby] + "<br />"; 
            }
          });
    $(this).popover('show')
  };

  var hideTooltip = function(d,i) {
    $('.popover').each(function() {
      $(this).remove();
    }); 
  };

  function labels (centers) {
    svg.selectAll(".label").remove();

    svg.selectAll(".label")
    .data(centers).enter().append("text")
    .attr("class", "label")
    .text(function (d) { 
      var text = d.name === "" ? "empty string" : d.name;
      var len = d.name.length;
      if (len > truncateLen)
        text = d.name.substring(0,truncateLen) + "...";
      return text; 
    })
    .attr("transform", function (d) {
      return "translate(" + (d.x + (d.dx / 3)) + ", " + (d.y + 20) + ")"; // 
    });
  }

  // this returns extended objects
  var node = svg.selectAll('.node')
     .data(force.nodes())
     .enter().append('g')
     .attr('class', "node")

  node
      .append('circle')
      .attr('r', function(d) {
        return d.radius || 5
      }).style("fill", function(d) {
        return colors(d[thedata.feature]);
      })

  node.on("mouseover", function(d) {
    showTooltip.call(this,d);
  }).on("mouseout", hideTooltip);

  var centers = getCenters([width - margin.right, height]);
  force.on("tick", tick(centers, thedata.groupby));
  labels(centers)
  force.start();

  // keep legend for later use
  var legend = svg.selectAll(".legend")
      .data(colors.domain())
      .enter().append("g")
      .attr("class", "legend")
      .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });

  legend.append("rect")
      .attr("x", legendX - 18)
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", colors);

  legend.append("text")
      .attr("x", legendX - 24)
      .attr("y", 9)
      .attr("dy", ".35em")
      .style("text-anchor", "end")
      .text(function(d) { 
        var text = d === "" ? "empty string" : d;
        var len = d.length;
        if (len > truncateLen)
          text = d.substring(0,truncateLen) + "...";
        return text; 
      });

  function tick(centers, varname) {
    var foci = {};
    for (var i = 0; i < centers.length; i++) {
      foci[centers[i].name] = centers[i];
    }
    return function (e) {
      for (var i = 0; i < data.length; i++) {
        var o = data[i];
        var f = foci[o[varname]];
        o.y += ((f.y + (f.dy / 2)) - o.y) * e.alpha;
        o.x += ((f.x + (f.dx / 2)) - o.x) * e.alpha;
      }
      node.attr("transform", function(d) {
        var r = d.radius;
        d.x = Math.max(r, Math.min(totalw - r, d.x));
        d.y = Math.max(r, Math.min(totalh - r, d.y));
        return "translate(" + d.x + "," + d.y + ")"; 
      });
    }
  }

  function tick1() {
    node.attr("transform", function(d) {
      var r = d.radius;
      d.x = Math.max(r, Math.min(totalw - r, d.x));
      d.y = Math.max(r, Math.min(totalh - r, d.y));
      return "translate(" + d.x + "," + d.y + ")"; 
    });
  }
  
};

// Tell Shiny about the output binding
Shiny.outputBindings.register(d3_bubbles_clusters_bind, "d3-bubbles-clusters");

var d3_box_bind = new Shiny.OutputBinding();

d3_box_bind.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".d3-box-plot");
};

d3_box_bind.renderValue = function(el, thedata) {

  var margin = {top: 20, right: 90, bottom: 90, left: 100};
  var totalw = 1300;
  var totalh = 450;
  var width = totalw - margin.left - margin.right;
  var height = totalh - margin.top - margin.bottom;
    
  var min = Infinity,
      max = -Infinity;

  var index = 0;
  var data = [];
  // var features = [];
  for(group in thedata.data) {
    data[index] = [];
    var arr = thedata.data[group];
    var rowMax = _.max(arr);
    var rowMin = _.min(arr);
    data[index][0] = group;
    // data[index][1] = [];
    data[index][1] = arr;

    if (rowMax > max) max = rowMax;
    if (rowMin < min) min = rowMin;

    index++;
  }

  // show the text labels beside individual boxplots if < 10 groups
  var labels = false; 
  if (index < 10)
    labels = true;

  var chart = d3.box()
    .whiskers(iqr(1.5))
    .height(height) 
    .domain([min, max])
    .showLabels(labels);

  // remove the old graph
  var svg = d3.select(el).select("svg");
  svg.remove();
  $(el).html("");

  // append a new one
  svg = d3.select(el).append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .attr("class", "box")    
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  
  // the x-axis
  var x = d3.scale.ordinal()     
    .domain( data.map(function(d) { console.log(d); return d[0] } ) )     
    .rangeRoundBands([0 , width], 0.7, 0.3);    

  var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");

  // the y-axis
  var y = d3.scale.linear()
    .domain([min, max])
    .range([height + margin.top, 0 + margin.top]);
  
  var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left");

  // draw the boxplots  
  svg.selectAll(".box")    
      .data(data)
    .enter().append("g")
    .attr("transform", function(d) { return "translate(" +  x(d[0])  + "," + margin.top + ")"; } )
      .call(chart.width(x.rangeBand())); 
 
   // draw y axis
  svg.append("g")
        .attr("class", "y axis")
        .call(yAxis)
    .append("text") // and text1
      .attr("transform", "rotate(-90)")
      .attr("x", -(height / 2) )
      .attr("y", 0)
      .attr("dy", "-3.5em")
      .style("text-anchor", "middle")
      .style("font-size", "16px") 
      .text(thedata.dist);    
  
  // draw x axis  
  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + (height  + margin.top + 10) + ")")
      .call(xAxis)
    .append("text") // text label for the x axis
        .attr("x", (width / 2) )
        .attr("y",  10 )
    .attr("dy", "1.71em")
        .style("text-anchor", "middle")
    .style("font-size", "16px") 
        .text(thedata.feature); 

  // Returns a function to compute the interquartile range.
  function iqr(k) {
    return function(d, i) {
      var q1 = d.quartiles[0],
          q3 = d.quartiles[2],
          iqr = (q3 - q1) * k,
          i = -1,
          j = d.length;
      while (d[++i] < q1 - iqr);
      while (d[--j] > q3 + iqr);
      return [i, j];
    };
  }
};

// Tell Shiny about the output binding
Shiny.outputBindings.register(d3_box_bind, "d3-box-plot");

// keep bubbles multi groups chart for later use
// var d3_bubbles_multi_bind = new Shiny.OutputBinding();

// d3_bubbles_multi_bind.find = function(scope) {
//   // For the given scope, return the set of elements that belong to
//   // this binding.
//   return $(scope).find(".d3-bubbles-multi");
// };

// d3_bubbles_multi_bind.renderValue = function(el, thedata) {
//   debugger
//   var featureID = thedata.ylabel;
//   var data = [];
//   // var minmax = [thedata.range.min,thedata.range.max];
//   // var totalw = $(el).width();
//   // var totalh = $(el).height();
//   var totalw = 850;
//   var totalh = 450;
//   var margin = {
//     top:30,
//     bottom:30,
//     left:30,
//     right:30
//   };

//   var colors = d3.scale.category20();

//   //remove the old graph
//   var svg = d3.select(el).select("svg");
//   svg.remove();
//   $(el).html("");

//   //append a new one
//   svg = d3.select(el)
//           .append("svg")
//           .attr("height", totalh) //- margin.top - margin.bottom
//           .attr("width", totalw) //- margin.top - margin.bottom

//   function convertRange( value, r1, r2 ) { 
//       return ( value - r1[ 0 ] ) * ( r2[ 1 ] - r2[ 0 ] ) / ( r1[ 1 ] - r1[ 0 ] ) + r2[ 0 ];
//   }
  
//   colors("Original category")
//   // colors("NAs")
//   // colors("Empty Strings")
//   // colors("Clubbed category")

//   // Like other layouts, d3 expects objects that the layout can manipulate
//   thedata.data[thedata.feature].forEach(function(item, index) {
//     var r = 5;

//     var color = colors("Original category")

//     data[index] = {
//       // label:item,
//       value: 1,
//       radius: r,
//       color:color
//     }
//     data[index][thedata.feature] = item;
//     data[index][thedata.groupby] = thedata.data[thedata.groupby][index];
//   })

//   var getCenters = function (size) {
//     var centers, map;
//     centers = $.unique(thedata.data[thedata.groupby]).map(function (d) {
//       return {name: d, value: 1};
//     });
    
//     map = d3.layout.treemap().size(size).ratio(1/1);
//     map.nodes({children: centers});

//     // map = d3.layout.pack().size(size);
//     // map.nodes({children: centers}); 

//     return centers;
//   };

//   var force = d3.layout.force()
//       .nodes(data)

//       // size affects center of gravity
//       .size([totalw, totalh])

//       // defaults to .9
//       // 1 = frictionless, 0 = frozen
//       .friction(0.9)

//       // defaults to -30
//       // can be a function
//       // A negative value results in node repulsion, while a positive value results in node attraction.
//       // 0 charge disables the quadtree computation
//       .charge(function(d) { 
//         return -(d.radius * d.radius)/0.8; 
//         // return -d.radius;
//         // return -100;
//       })

//       // deftaults to 0.8
//       // sets approximation
//       .theta(1)
      
//       // defaults to 0.1
//       // 0 = disabled
//       .gravity(0.1)

//       // gets or sets the cooling param
//       .alpha(0.9)

//   var showTooltip = function(d,i) {
//     $(this).popover({
//             placement: 'auto top',
//             container: 'body',
//             trigger: 'manual',
//             html : true,
//             content: function() { 
//               return "<b>Feature:</b> " + d[thedata.feature] + "<br />" + 
//                         "<b>Grouped By:</b> " + d[thedata.groupby] + "<br />"; 
//             }
//           });
//     $(this).popover('show')
//   };

//   var hideTooltip = function(d,i) {
//     $('.popover').each(function() {
//       $(this).remove();
//     }); 
//   };

//   function labels (centers) {
//     svg.selectAll(".label").remove();

//     svg.selectAll(".label")
//     .data(centers).enter().append("text")
//     .attr("class", "label")
//     .text(function (d) { return d.name })
//     .attr("transform", function (d) {
//       return "translate(" + (d.x + (d.dx / 2)) + ", " + (d.y + 20) + ")";
//     });
//   }

//   // this returns extended objects
//   var node = svg.selectAll('.node')
//      .data(force.nodes())
//      .enter().append('g')
//      .attr('class', "node")

//   node
//       .append('circle')
//       .attr('r', function(d) {
//         return d.radius || 5
//       }).style("fill", function(d) {
//         return d.color;
//       })

//   node.on("mouseover", function(d) {
//     showTooltip.call(this,d);
//   }).on("mouseout", hideTooltip);

//   var centers = getCenters([totalw-margin.left-margin.right, totalh-margin.top-margin.bottom]);
//   force.on("tick", tick(centers, thedata.groupby));
//   // labels(centers)
//   force.start();

//   var legend = svg.selectAll(".legend")
//       .data(colors.domain())
//       .enter().append("g")
//       .attr("class", "legend")
//       .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });

//   legend.append("rect")
//       .attr("x", totalw - 18)
//       .attr("width", 18)
//       .attr("height", 18)
//       .style("fill", colors);

//   legend.append("text")
//       .attr("x", totalw - 24)
//       .attr("y", 9)
//       .attr("dy", ".35em")
//       .style("text-anchor", "end")
//       .text(function(d) { return d; });

//   function tick(centers, varname) {
//     var foci = {};
//     for (var i = 0; i < centers.length; i++) {
//       foci[centers[i].name] = centers[i];
//     }
//     return function (e) {
//       for (var i = 0; i < data.length; i++) {
//         var o = data[i];
//         var f = foci[o[varname]];
//         o.y += ((f.y + (f.dy / 2)) - o.y) * e.alpha;
//         o.x += ((f.x + (f.dx / 2)) - o.x) * e.alpha;
//       }
//       node.attr("transform", function(d) {
//         var r = d.radius;
//         d.x = Math.max(r, Math.min(totalw - r, d.x));
//         d.y = Math.max(r, Math.min(totalh - r, d.y));
//         return "translate(" + d.x + "," + d.y + ")"; 
//       });
//     }
//   }

//   function tick1() {
//     node.attr("transform", function(d) {
//       var r = d.radius;
//       d.x = Math.max(r, Math.min(totalw - r, d.x));
//       d.y = Math.max(r, Math.min(totalh - r, d.y));
//       return "translate(" + d.x + "," + d.y + ")"; 
//     });
//   }
  
// };

// // Tell Shiny about the output binding
// Shiny.outputBindings.register(d3_bubbles_multi_bind, "d3-bubbles-multi");

})();