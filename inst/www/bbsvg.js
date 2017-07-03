

var svg_id = "mysvg";
var el_id = "mypoint";

var el = $("#"+svg_id+" #"+el_id);

el.velocity({x: 150});
el.velocity({fill: "#ff0000"});

$("body").on("mousemove", "#"+svg_id,function(e) {
  var offset = $(this).offset();
  var x = (e.pageX - offset.left);
  var y = (e.pageY - offset.top);
  //alert("svg click: x= "+x+"y="+y);
  
  var el = $("#mypoint");
  el.attr({cx: x, cy: y});  
});  
