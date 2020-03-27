$(document).ready(function(){
  $(".mycolIcon").click(function(args){
    var target = args.dataset.target;
    var target2 = args.parentNode.parentNode;
    if ($(target).hasClass("collapsing")){
      return;
    }
    $(target2).toggleClass("collsidebar");
    
  });
});
