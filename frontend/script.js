console.log("JavaScript is loaded");

let lastScrollTop = 0; // Track the last scroll position
const header = document.getElementById('header');
const footer = document.getElementById('footer');

window.addEventListener('scroll', function() {
    let currentScroll = window.scrollY || document.documentElement.scrollTop;

    // If scrolling down, hide the header and footer
    if (currentScroll > lastScrollTop) {
        header.style.transform = 'translateY(-100%)';  // Move header off screen
        footer.style.transform = 'translateY(100%)';  // Move footer off screen
    } else {
        // If scrolling up, show the header and footer
        header.style.transform = 'translateY(0)';
        footer.style.transform = 'translateY(0)';
    }

    lastScrollTop = currentScroll <= 0 ? 0 : currentScroll; // Avoid negative scroll
});