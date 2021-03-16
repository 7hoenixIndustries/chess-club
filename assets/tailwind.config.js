module.exports = {
  purge: {
      content: [
        "../**/*.html.eex",
        "../**/*.html.leex",
        "../**/views/**/*.ex",
        "../**/live/**/*.ex",
        "./js/**/*.js",
        './**/*.elm',
      ],
      // Dunno if we need this or not.
      options: {
        safelist: [/translate-x-*-squares/]
      }
  },
  darkMode: 'media', // or 'media' or 'class' or false
  theme: {
    extend: {
      gridTemplateRows: {
        // Simple 8 row grid
        '8': 'repeat(8, minmax(0, 1fr))',
      },
      gridTemplateCols: {
        // Simple 8 row grid
        '8': 'repeat(8, minmax(0, 1fr))',
      },
      width: {
        '1/8': '12.5%',
        'constrained-1/4': '25vw',
        'constrained-40%': '40vw',
        'constrained-1/2': '50vw',
        'constrained-1': '100vw'
      },
      height: {
        '1/8': '12.5%',
        'constrained-1/4': '25vw',
        'constrained-40%': '40vw',
        'constrained-1/2': '50vw',
        'constrained-1': '100vw'
      }
    }
  },
  variants: {
    extend: {},
  },
  plugins: [
//    require('@tailwindcss/forms')
  ],
}
